*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section


 TYPES:
 BEGIN OF ty_header,
   STRUCTURE TYPE zmmt_ee_zgr,
   END OF ty_header,

   BEGIN OF ty_doc_fiscal,
     docnum TYPE j_1bnfdoc-docnum,
     cancel TYPE j_1bnfdoc-cancel,
     candat TYPE j_1bnfdoc-candat,
   END OF ty_doc_fiscal,

   BEGIN OF ty_doc_fatura,
     belnr TYPE rbkp-belnr,
     gjahr TYPE rbkp-gjahr,
     stblg TYPE rbkp-stblg,
   END OF ty_doc_fatura,

   BEGIN OF ty_doc_mat,
     mblnr     TYPE mkpf-mblnr,
     mjahr     TYPE mkpf-mjahr,
     estornado TYPE char1,
   END OF ty_doc_mat,

   BEGIN OF ty_doc_mat_sob,
     mblnr     TYPE mkpf-mblnr,
     mjahr     TYPE mkpf-mjahr,
     estornado TYPE char1,
   END OF ty_doc_mat_sob,

   BEGIN OF ty_doc_aviso,
     vbeln     TYPE vbeln,
     eliminado TYPE char1,
   END OF ty_doc_aviso,

   BEGIN OF ty_response,
     ZMMT_EE_ZGR        TYPE zmmt_ee_zgr,
     obj_key       TYPE awkey,
     doc_fiscal    TYPE ty_doc_fiscal,
     doc_fatura    TYPE ty_doc_fatura,
     doc_material  TYPE ty_doc_mat,
     doc_mat_sobra TYPE ty_doc_mat_sob,
     doc_aviso     TYPE ty_doc_aviso,
   END OF ty_response.

 TYPES: zde_data_response TYPE ty_response .
