/* ************************************************************************ **
** This file is part of rsp2, and is licensed under EITHER the MIT license  **
** or the Apache 2.0 license, at your option.                               **
**                                                                          **
**     http://www.apache.org/licenses/LICENSE-2.0                           **
**     http://opensource.org/licenses/MIT                                   **
**                                                                          **
** Be aware that not all of rsp2 is provided under this permissive license, **
** and that the project as a whole is licensed under the GPL 3.0.           **
** ************************************************************************ */

extern crate proc_macro;

use syn::{parse, parse_quote};
use quote::quote;
use proc_macro2::{Span, TokenStream};

use std::mem;

#[proc_macro_attribute]
pub fn config_type(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    config_type_(attr.into(), item.into()).into()
}

/// Implementation of `config_type_` for producing customized versions of
/// `#[derive(Serialize, Deserialize)]`.
///
/// Here's the general idea of what we do:
///
/// * The type itself won't have `#[derive(Serialize, Deserialize)]`;
///   it needs custom `Serialize`/`Deserialize` impls that THIS macro will generate.
/// * However, we still want to delegate the bulk of the implementation to serde's derives.
///   Therefore, we need to generate two additional types that have these derives.
///
/// ```text
/// #[test_config]
/// struct Foo {
///     #[serde(rename = "baz")]
///     bar: i32,
/// }
/// ```
///
/// becomes
///
/// ```rust
/// # use std::marker::PhantomData;
/// struct Foo {
///     bar: i32,
/// }
///
/// #[derive(Serialize)]
/// struct Ser<'a> {
///     #[serde(rename = "baz")]
///     bar: &'a i32,
///     #[serde(skip)]
///     _phantom: PhantomData<&'a ()>, // in case of 0 fields
/// }
///
/// #[derive(Deserialize)]
/// struct De {
///     #[serde(rename = "baz")]
///     bar: i32,
/// }
/// ```
fn config_type_(attr: TokenStream, item: TokenStream) -> TokenStream {
    if let Some(token) = attr.into_iter().next() {
        panic!("{}", token);
    }

    // this macro may not be a custom derive, but DeriveInput is the most convenient description
    // of what it takes
    let mut item: syn::DeriveInput = syn::parse2(item).unwrap();
    let ref ident = item.ident.clone();

    // TODO: Either require that `#[derive(Serialize, Deserialize)]` are present, or forbid them.

    // Define two additional types with the same set of fields.
    let mut ser = item.clone();
    let mut de = item.clone();
    ser.ident = parse_quote!{ Ser };
    de.ident = parse_quote!{ De };

    // Give them derive attributes
    ser.attrs.insert(0, parse_quote!{ #[derive(Serialize)] });
    de.attrs.insert(0, parse_quote!{ #[derive(Deserialize)] });

    // Ser needs a lifetime
    let ref lifetime: syn::Lifetime = parse_quote!{ 'config_proc_macro };
    ser.generics.params.insert(0, parse_quote!{ #lifetime });

    // `#[serde]` attributes are only relevant to the new types.
    // (and it is unlikely that any other attributes are needed by them)
    //
    // (FIXME: It seems like it should be cleaner to do this using VisitMut, rather than having
    //         to do this at so many different places in this function; but there's no method
    //         on VisitMut for visiting a `Vec<Attribute>`)
    remove_serde_attributes(&mut item.attrs);
    remove_non_serde_attributes(&mut ser.attrs);
    remove_non_serde_attributes(&mut de.attrs);

    let (_, ty_generics, where_clause) = &item.generics.split_for_impl();
    let (ser_impl_generics, ser_ty_generics, _) = &item.generics.split_for_impl();
    let (de_impl_generics, de_ty_generics, _) = &item.generics.split_for_impl();

    let into_ser_impl: syn::Item;
    let from_de_impl: syn::Item;

    let serialize_impl: syn::Item;
    let deserialize_impl: syn::Item;

    match &mut item.data {
        // FIXME: branch body too big
        syn::Data::Struct(data) => {
            if let syn::Fields::Unit = data.fields {
                panic!("unit structs not supported");
            }

            // FIXME: Is there a better way to traverse these three structures in lockstep?
            let ser_data = match &mut ser.data { syn::Data::Struct(x) => x, _ => unreachable!(), };
            let de_data = match &mut de.data { syn::Data::Struct(x) => x, _ => unreachable!(), };

            // (NOTE: can't use zip_eq! due to a bug in syn; size_hint isn't overridden)
            let iter = data.fields.iter_mut().zip(&mut ser_data.fields).zip(&mut de_data.fields);
            for ((field, ser_field), de_field) in iter {
                remove_serde_attributes(&mut field.attrs);
                remove_non_serde_attributes(&mut ser_field.attrs);
                remove_non_serde_attributes(&mut de_field.attrs);

                let ty = mem::replace(&mut ser_field.ty, parse_quote!{ () });
                ser_field.ty = parse_quote!{ & #lifetime #ty };
            }

            // Field names (or integer names, for tuple structs)
            let ref members: Vec<syn::Member> = fields_to_members(&data.fields).expect("not unit");

            // Ser also gets a PhantomData member in case it has no fields.
            let phantom_type: syn::Type = parse_quote!{ std::marker::PhantomData<&#lifetime () >};
            let phantom_expr: syn::Expr = parse_quote!{ std::marker::PhantomData };
            let phantom_member: syn::Member;
            match &mut ser_data.fields {
                syn::Fields::Named(ser_fields) => {
                    let ref phantom_ident: syn::Ident = parse_quote!{ _config_type_lifetime };
                    phantom_member = parse_quote!{ #phantom_ident };

                    let NamedField(phantom_field) = parse_quote!{
                        #[serde(skip)]
                        #phantom_ident: #phantom_type
                    };
                    ser_fields.named.push(phantom_field);
                },
                syn::Fields::Unnamed(ser_fields) => {
                    phantom_member = index_member(ser_fields.unnamed.len());

                    let UnnamedField(phantom_field) = parse_quote!{
                        #[serde(skip)] #phantom_type
                    };
                    ser_fields.unnamed.push(phantom_field);
                },
                syn::Fields::Unit => unreachable!(),
            }

            // Conversion of &Self -> Ser<'_>
            into_ser_impl = parse_quote!{
                impl #ser_impl_generics
                From< &#lifetime #ident #ty_generics > for Ser #ser_ty_generics
                #where_clause {
                    fn from(it: &#lifetime #ident #ty_generics) -> Self {
                        let #ident { #(#members),* } = it;
                        Ser { #(#members,)* #phantom_member: #phantom_expr }
                    }
                }
            };

            // Conversion of De -> Self
            from_de_impl = parse_quote!{
                impl #de_impl_generics
                From< De #de_ty_generics > for #ident #ty_generics
                #where_clause {
                    fn from(it: De #de_ty_generics) -> Self {
                        let De { #(#members),* } = it;
                        #ident { #(#members),* }
                    }
                }
            };

            // FIXME TODO
            // the customized Serialize/Deserialize impls
            serialize_impl = parse_quote!{ fn _lol() {} };
            deserialize_impl = parse_quote!{ fn _rofl() {} };
        },
        syn::Data::Enum(syn::DataEnum { .. }) => unimplemented!(),
        syn::Data::Union(syn::DataUnion { .. }) => panic!("unions are not supported"),
    }

    quote!{
        #item

        const _: () = {
            #serialize_impl
            #deserialize_impl

            #ser
            #de

            #into_ser_impl
            #from_de_impl
        }
    }
}

// field names or numbers, intended to be used inside `#( { #()* } )?`
fn fields_to_members(fields: &syn::Fields) -> Option<Vec<syn::Member>> {
    match fields {
        syn::Fields::Named(fields) => Some({
            fields.named.iter().map(|x| {
                let ident = &x.ident;
                parse_quote!{ #ident }
            }).collect()
        }),
        syn::Fields::Unnamed(fields) => Some((0..fields.unnamed.len()).map(index_member).collect()),
        syn::Fields::Unit => None,
    }
}

fn remove_serde_attributes(attrs: &mut Vec<syn::Attribute>) -> Vec<syn::Attribute> {
    remove_if(attrs, |attr| attr.path.is_ident("serde"))
}

fn remove_non_serde_attributes(attrs: &mut Vec<syn::Attribute>) -> Vec<syn::Attribute> {
    remove_if(attrs, |attr| !attr.path.is_ident("serde"))
}

fn remove_if<T>(vec: &mut Vec<T>, mut pred: impl FnMut(&T) -> bool) -> Vec<T> {
    let mut out = vec![];
    for i in (0..vec.len()).rev() {
        if pred(&vec[i]) {
            out.push(vec.remove(i));
        }
    }
    out.reverse();
    out
}

fn index_member(index: usize) -> syn::Member {
    syn::Member::Unnamed(syn::Index { index: index as u32, span: Span::call_site() })
}

// Desired output
/*
    #[derive(Debug, Clone, PartialEq)]
    pub struct LammpsPotentialKolmogorovCrespiFull {
        pub rebo: bool,

        /// Cutoff radius (Angstrom?)
        pub cutoff: OrDefault<f64>,

        /// Parameterization.
        pub params: LammpsKolmogorovCrespiParams,
    }

    const _: () = {
        use super::*;

        impl serde::Serialize for LammpsPotentialKolmogorovCrespiFull {
            fn serialize<S: ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                <Ref<'_> as serde::Serialize>::serialize(&self.into(), serializer)
            }
        }

        impl<'de> serde::Deserialize<'de> for LammpsPotentialKolmogorovCrespiFull {
            fn deserialize<D: de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
                <Owned as serde::Deserialize>::deserialize(deserializer).map(Into::into)
            }
        }

        impl<'a> Into<Ref<'a>> for &'a LammpsPotentialKolmogorovCrespiFull {
            fn into(self) -> Ref<'a> {
                let LammpsPotentialKolmogorovCrespiFull { rebo, cutoff, params } = self;
                Ref { rebo, cutoff, params }
            }
        }

        impl From<Owned> for LammpsPotentialKolmogorovCrespiFull {
            fn from(it: Owned) -> Self {
                let Owned { rebo, cutoff, params } = it;
                LammpsPotentialKolmogorovCrespiFull { rebo, cutoff, params }
            }
        }

        #[derive(Serialize)]
        #[derive(Debug, Clone, PartialEq)]
        pub struct Ref<'a> {
            #[serde(default = "potential__lammps_kolmogorov_crespi_full__rebo")]
            #[serde(skip_serializing_if = "potential__lammps_kolmogorov_crespi_full__rebo__skip")]
            pub rebo: &'a bool,

            #[serde(skip_serializing_if = "Option::is_none")]
            pub cutoff: &'a OrDefault<f64>,

            #[serde(default = "potential__lammps_kolmogorov_crespi_full__params")]
            pub params: &'a LammpsKolmogorovCrespiParams,
        }

        #[derive(Deserialize)]
        #[serde(rename_all = "kebab-case")]
        pub struct Owned {
            #[serde(default = "potential__lammps_kolmogorov_crespi_full__rebo")]
            #[serde(skip_serializing_if = "potential__lammps_kolmogorov_crespi_full__rebo__skip")]
            pub rebo: bool,

            #[serde(skip_serializing_if = "Option::is_none")]
            pub cutoff: OrDefault<f64>,

            #[serde(default = "potential__lammps_kolmogorov_crespi_full__params")]
            pub params: LammpsKolmogorovCrespiParams,
        }
    }
*/

struct NamedField(syn::Field);
struct UnnamedField(syn::Field);

impl parse::Parse for NamedField {
    fn parse(input: parse::ParseStream) -> parse::Result<Self> {
        syn::Field::parse_named(input).map(NamedField)
    }
}

impl parse::Parse for UnnamedField {
    fn parse(input: parse::ParseStream) -> parse::Result<Self> {
        syn::Field::parse_unnamed(input).map(UnnamedField)
    }
}


