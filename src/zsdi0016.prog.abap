*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDI0016                                                *
* Descrição  : Relatório de Controle Prazo R.Form.Lote                 *
* Módulo     : SD                                Transação: ZSDT0034   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 17/06/2011 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zsdi0016 NO STANDARD PAGE HEADING MESSAGE-ID sd.

*----------------------------------------------------------------------*
*                                Type Pools                            *
*----------------------------------------------------------------------*
TYPE-POOLS icon.

TABLES: t001 ,
        t001w,
        likp ,
        lips ,
        j_1bnfnad,
        j_1bnflin,
        zsdt0053,
        zsdt_export,
        zib_nfe_dist_ter.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_zsdt0002,
         fkart TYPE zsdt0002-fkart,
       END   OF type_zsdt0002,

       BEGIN OF ty_zfiwrt0008,
         docnum   TYPE zfiwrt0008-docnum,
         parid    TYPE zfiwrt0008-parid,
         seq_lcto TYPE zfiwrt0008-seq_lcto,
       END OF ty_zfiwrt0008,

       BEGIN OF ty_zfiwrt0009,
         seq_lcto TYPE zfiwrt0009-seq_lcto,
         itmnum   TYPE zfiwrt0009-itmnum,
         lgort    TYPE zfiwrt0009-lgort,
       END OF ty_zfiwrt0009,

       BEGIN OF type_likp,
         vbeln TYPE likp-vbeln,
         erdat TYPE likp-erdat,
         vstel TYPE likp-vstel,
         vkorg TYPE likp-vkorg,
         kunnr TYPE likp-kunnr,
         fkarv TYPE likp-fkarv,
       END   OF type_likp,

       BEGIN OF type_lips,
         vbeln TYPE lips-vbeln,
         posnr TYPE lips-posnr,
         matnr TYPE lips-matnr,
         lgort TYPE lips-lgort,
         vgbel TYPE lips-vgbel,
       END   OF type_lips,

       BEGIN OF type_vbrp,
         vbeln  TYPE vbrp-vbeln,
         posnr  TYPE vbrp-posnr,
         vgbel  TYPE vbrp-vgbel,
         vgpos  TYPE vbrp-vgpos,
         vgtyp  TYPE vbrp-vgtyp,
         refkey TYPE j_1bnflin-refkey,
       END   OF type_vbrp,

       BEGIN OF type_lin,
         docnum TYPE j_1bnflin-docnum,
         itmnum TYPE j_1bnflin-itmnum,
         matnr  TYPE j_1bnflin-matnr,
         maktx  TYPE j_1bnflin-maktx,
         refkey TYPE j_1bnflin-refkey,
         refitm TYPE j_1bnflin-refitm,
         reftyp TYPE j_1bnflin-reftyp,
         menge  TYPE j_1bnflin-menge,
         charg  TYPE j_1bnflin-charg,
         nbm    TYPE j_1bnflin-nbm,
       END   OF type_lin,

       BEGIN OF type_ov,
         vbeln TYPE vbrp-vbeln,
         posnr TYPE vbrp-posnr,
       END   OF type_ov,

       BEGIN OF type_doc,
         docnum TYPE j_1bnfdoc-docnum,
         docdat TYPE j_1bnfdoc-docdat,
         bukrs  TYPE j_1bnfdoc-bukrs,
         branch TYPE j_1bnfdoc-branch,
         parid  TYPE j_1bnfdoc-parid,
         series TYPE j_1bnfdoc-series,
         nfnum  TYPE j_1bnfdoc-nfnum,
         nfenum TYPE j_1bnfdoc-nfenum,
       END   OF type_doc,

       BEGIN OF type_lfa1,
         lifnr TYPE lfa1-lifnr,
         land1 TYPE lfa1-land1,
         name1 TYPE lfa1-name1,
         regio TYPE lfa1-regio,
       END   OF type_lfa1,

       BEGIN OF type_retlote,
         docnum       TYPE zsdt_retlote-docnum,
         nfenum       TYPE zsdt_retlote-nfenum,
         werks        TYPE zsdt_retlote-werks,
         nf_retorno   TYPE zsdt_retlote-nf_retorno,
         docnum_ret   TYPE zsdt_retlote-docnum_ret,
         quant_vinc   TYPE zsdt_retlote-quant_vinc,
         data_criacao TYPE zsdt_retlote-data_criacao,
         id_export    TYPE zsdt_retlote-id_export, "81360
       END   OF type_retlote,

       BEGIN OF type_saida,
         docnum             TYPE j_1bnfdoc-docnum,
         itmnum             TYPE j_1bnflin-itmnum,
         bukrs              TYPE j_1bnfdoc-bukrs,
         branch             TYPE j_1bnfdoc-branch,
         nfenum             TYPE j_1bnfdoc-nfenum,
         series             TYPE j_1bnfdoc-series,
         docdat             TYPE j_1bnfdoc-docdat,
         parid              TYPE j_1bnfdoc-parid,
         name1              TYPE lfa1-name1,
         regio              TYPE lfa1-regio,
         matnr              TYPE j_1bnflin-matnr,
         maktx              TYPE j_1bnflin-maktx,
         menge              TYPE j_1bnflin-menge,
         q_vinc             TYPE zsdt_retlote-quant_vinc,
         saldo              TYPE ccm_quant,
         saldo_ico          TYPE ccm_quant,
         dias               TYPE zdias,
         final              TYPE char30,
         doc_ret            TYPE j_1bnfdoc-docnum,
         doc_exp            TYPE j_1bnfdoc-docnum,
         quant              TYPE j_1bnetqty,
         icon               TYPE icon-name,
         lgort              TYPE lips-lgort,
         charg              TYPE j_1bnflin-charg,
         parid_nad          TYPE j_1bnfnad-parid,
         name1_nad          TYPE j_1bnfnad-name1,
         nf_retorno         TYPE zsdt_retlote-nf_retorno,
         nfenumexp          TYPE j_1bnfdoc-nfenum,
         dt_recepcao        TYPE zlest0146-dt_recepcao,
         peso_aferido       TYPE zlest0146-peso_aferido_recepcao,
         peso_fiscal_cct    TYPE zlest0146-peso_aferido_recepcao,
         dif_peso_cct       TYPE zlest0146-peso_aferido_recepcao,
         saldo_psaf_psfi    TYPE zlest0146-peso_aferido_recepcao, "Saldo Peso aferido e Quantidade Vinculada
         nbm                TYPE j_1bnflin-nbm,
         chave_nfe          TYPE zib_nfe_forn-nu_chave,
         chavenfeexp        TYPE zib_nfe_forn-nu_chave,
         chavedocret        TYPE zib_nfe_forn-nu_chave,
         conf_cct_portal    TYPE c LENGTH 4,
         term_cct_portal    TYPE zsdt0168-lifnr,
         ds_term_cct_portal TYPE lfa1-name1,
         dt_recepcao_portal TYPE zlest0186-dt_recepcao,
         remessa            TYPE likp-vbeln,
         vgbel              TYPE lips-vgbel,
         id_due             TYPE zdoc_exp-id_due,
         numero_due         TYPE zsdt0170-numero_due,
         docdatret          TYPE j_1bdocdat,
         docdatexp          TYPE j_1bdocdat,
         chave_acesso       TYPE zde_chv_acesso_due,
         ds_situacao_due    TYPE dd07t-ddtext,
         redex              TYPE zsdt_export-redex,
         sld_dev            TYPE zib_nfe_dist_itm-prod_qtd_comerci,
         id_redex           TYPE zsdt_export-id_export,
         ds_nome_transpor   TYPE znom_transporte-ds_nome_transpor,
       END   OF type_saida,

       BEGIN OF type_vbfa,
         vbelv   TYPE vbfa-vbelv,
         posnv   TYPE vbfa-posnv,
         vbeln   TYPE vbfa-vbeln,
         posnn   TYPE vbfa-posnn,
         vbtyp_n TYPE vbfa-vbtyp_n,
         vbtyp_v TYPE vbfa-vbtyp_v,
         refkey  TYPE j_1bnflin-refkey,
       END   OF type_vbfa,

       BEGIN OF ty_j_1bnfnad,
         parid  TYPE j_1bnfnad-parid,
         name1  TYPE j_1bnfnad-name1,
         docnum TYPE j_1bnfnad-docnum,
         parvw  TYPE j_1bnfnad-parvw,
       END OF ty_j_1bnfnad,

       BEGIN OF ty_doc_export,
         docnum_ret TYPE j_1bnfnad-docnum,
         docnum     TYPE j_1bdocnum,
*         docdat     TYPE
       END OF ty_doc_export,

       BEGIN OF ty_nfe_export,
         docnum TYPE j_1bdocnum,
         nfenum TYPE j_1bnfnum9,
       END OF ty_nfe_export,

       BEGIN OF ty_nfe,
         regio   TYPE  j_1bnfe_active-regio,
         nfyear  TYPE  j_1bnfe_active-nfyear,
         nfmonth TYPE  j_1bnfe_active-nfmonth,
         stcd1   TYPE  j_1bnfe_active-stcd1,
         model   TYPE  j_1bnfe_active-model,
         serie   TYPE  j_1bnfe_active-serie,
         nfnum9  TYPE  j_1bnfe_active-nfnum9,
         docnum9 TYPE  j_1bnfe_active-docnum9,
         cdv     TYPE  j_1bnfe_active-cdv,
       END OF ty_nfe,

       BEGIN OF ty_docdat,
         docnum TYPE j_1bnfdoc-docnum,
         docdat TYPE j_1bnfdoc-docdat,
       END OF ty_docdat.

TYPES: BEGIN OF ty_zcarta.
         INCLUDE TYPE zcarta_correcao.
TYPES: END OF ty_zcarta.

TYPES: BEGIN OF ty_zsdt_export.
         INCLUDE TYPE zsdt_export.
TYPES chavedocret TYPE zib_nfe_forn-nu_chave.
TYPES: END OF ty_zsdt_export.




*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_zsdt0002        TYPE TABLE OF type_zsdt0002,
      it_zfiwrt0008     TYPE TABLE OF ty_zfiwrt0008,
      wa_zfiwrt0008     TYPE          ty_zfiwrt0008,
      it_zfiwrt0009     TYPE TABLE OF ty_zfiwrt0009,
      it_zfiwrt0015     TYPE TABLE OF zfiwrt0015 WITH HEADER LINE,
      t_lin             TYPE TABLE OF type_lin,
      t_lin2            TYPE TABLE OF type_lin,
      t_dd07t           TYPE TABLE OF dd07t,
      t_doc             TYPE TABLE OF type_doc,
      t_active          TYPE TABLE OF j_1bnfe_active,
      t_doc2            TYPE TABLE OF type_doc,
      t_likp            TYPE TABLE OF type_likp,
      t_lips            TYPE TABLE OF type_lips,
      t_vbrp            TYPE TABLE OF type_vbrp,
      t_lfa1            TYPE TABLE OF type_lfa1,
      t_retlote         TYPE TABLE OF type_retlote,
      t_sumret          TYPE TABLE OF type_retlote,
      t_zsdt0033        TYPE TABLE OF zsdt0033,
      t_export          TYPE TABLE OF zsdt_export,
      t_doc_export      TYPE TABLE OF ty_doc_export,
      t_nfe_export      TYPE TABLE OF ty_nfe_export,
      t_vbfa            TYPE TABLE OF type_vbfa,
      t_vbfaexp         TYPE TABLE OF vbfa,
      t_zdoc_exp        TYPE TABLE OF zdoc_exp,
      t_zsdt0170        TYPE TABLE OF zsdt0170,
      t_saida           TYPE TABLE OF type_saida,
      t_saida_aux       TYPE TABLE OF type_saida,
      t_fcat            TYPE TABLE OF lvc_s_fcat,
      t_j_1bnfnad       TYPE TABLE OF ty_j_1bnfnad,
      t_zsdt0053        TYPE TABLE OF zsdt0053,
      t_doc_ret         TYPE TABLE OF ty_docdat,
      t_doc_exp         TYPE TABLE OF ty_docdat,
      t_activeexp       TYPE TABLE OF j_1bnfe_active,
      it_parce_car      TYPE TABLE OF ty_j_1bnfnad,
      wa_parce_car      TYPE          ty_j_1bnfnad,
      it_zcarta         TYPE TABLE OF ty_zcarta,
      wa_zcarta         TYPE          ty_zcarta,
      t_tool            TYPE          ui_functions,
      s_cont            TYPE REF TO   cl_gui_custom_container,
      s_alv             TYPE REF TO   cl_gui_alv_grid,
      sl_zdoc_exp       TYPE          zdoc_exp,
      sl_activeexp      TYPE j_1bnfe_active,
      sl_zsdt0170       TYPE zsdt0170,
      sl_zsdt0053       TYPE zsdt0053,
      sl_doc_ret        TYPE ty_docdat,
      sl_vbfaexp        TYPE vbfa,
      sl_doc_exp        TYPE ty_docdat,
      s_layout          TYPE lvc_s_layo,
      gs_variant_c      TYPE disvariant,
      it_active         TYPE TABLE OF j_1bnfe_active,
      it_nfe            TYPE TABLE OF ty_nfe,
      it_ov             TYPE TABLE OF type_ov,
      t_znom_transporte TYPE TABLE OF znom_transporte.

DATA: variante        LIKE disvariant.
*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
CONSTANTS: c_j      TYPE char1  VALUE 'J',
           c_bi     TYPE char2  VALUE 'BI',
           c_table  TYPE char7  VALUE 'T_SAIDA',
           c_x      TYPE char1  VALUE 'X',
           c_nfenum TYPE char6  VALUE 'NFENUM',
           c_docret TYPE char7  VALUE 'DOC_RET',
           c_docexp TYPE char7  VALUE 'DOC_EXP'.

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
    s_bukrs  FOR t001-bukrs  NO INTERVALS NO-EXTENSION OBLIGATORY ,
    s_werks  FOR t001w-werks,
    s_matnr  FOR lips-matnr  NO INTERVALS NO-EXTENSION            ,
    s_matkl  FOR j_1bnflin-matkl                                  ,
    s_erdat  FOR likp-erdat                                       ,
    s_kunnr  FOR likp-kunnr                                       ,
    s_parid  FOR j_1bnfnad-parid                                  ,
    s_charg  FOR j_1bnflin-charg                                ,
    s_docnum FOR j_1bnflin-docnum                              ,
    s_chave  FOR zib_nfe_dist_ter-chave_nfe.

  PARAMETERS : s_final TYPE zsdt_export-finalidade.
SELECTION-SCREEN END   OF BLOCK a1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-037. "Processamento
  PARAMETERS: p_srvcct AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK b2.

PARAMETERS p_report(10) TYPE c NO-DISPLAY.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-000.
  PARAMETER: p_varia TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b3.

INITIALIZATION.
  gs_variant_c-report      = sy-repid.
**----------------------------------------------------------------------*
**                               Classes                                *
**----------------------------------------------------------------------*
  CLASS lcl_event_receiver DEFINITION DEFERRED.
  DATA s_event TYPE REF TO lcl_event_receiver.


*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION                            *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      zm_handle_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
          e_row_id e_column_id es_row_no.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION                        *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD zm_handle_hotspot.
*   HotSpot
    PERFORM z_handle_hotspot USING e_row_id
                                   e_column_id
                                   es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


DATA: vg_repid   LIKE sy-repid,
      vg_variant TYPE disvariant.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  vg_repid          = sy-repid.
  variante-report = vg_repid.

  IF ( NOT p_varia IS INITIAL ).
    vg_variant-variant = p_varia.

  ENDIF.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = variante
      i_save        = 'A'
    IMPORTING
      es_variant    = variante
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE variante-variant TO p_varia.
    MOVE variante-variant TO gs_variant_c-variant.
  ENDIF.

*----------------------------------------------------------------------*
*                           Start of Selection                         *
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Seleciona Dados
  PERFORM: z_seleciona_dados,

* Processa Dados
           z_processa_dados ,

* Monta FieldCat
           z_monta_fieldcat .

  CHECK NOT t_saida[] IS INITIAL.

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                             Seleciona Dados                          *
*----------------------------------------------------------------------*
FORM z_seleciona_dados.
  " seleciona dados nota Write que tem fom. de lote
  SELECT zfiwrt0008~docnum zfiwrt0008~parid zfiwrt0008~seq_lcto
    FROM zfiwrt0008
    INNER JOIN zfiwrt0001 ON zfiwrt0001~operacao = zfiwrt0008~operacao
                         AND zfiwrt0001~ctrl_zrfl = 'S'
    INNER JOIN j_1bnflin ON j_1bnflin~docnum = zfiwrt0008~docnum
    INTO TABLE it_zfiwrt0008
    WHERE zfiwrt0008~docnum GT 0
     AND  zfiwrt0008~budat  IN s_erdat
     AND  zfiwrt0008~branch IN s_werks
     AND  zfiwrt0008~bukrs  IN s_bukrs
     AND  zfiwrt0008~parid  IN s_kunnr
     AND  j_1bnflin~matnr   IN s_matnr.

  IF it_zfiwrt0008[] IS NOT INITIAL.





    SELECT *
      FROM zfiwrt0009 INTO CORRESPONDING FIELDS OF TABLE it_zfiwrt0009
      FOR ALL ENTRIES IN it_zfiwrt0008
     WHERE seq_lcto EQ it_zfiwrt0008-seq_lcto.
  ENDIF.


* Seleciona ZSDT0002
  PERFORM: z_seleciona_zsdt0002,

* Seleciona LIKP
           z_seleciona_likp    ,

* Seleciona LIPS
           z_seleciona_lips    ,

* Seleciona VBRP
           z_seleciona_vbrp    ,

* Seleciona J_1BNFLIN
           z_seleciona_lin     ,

* Seleciona J_1BNFDOC
           z_seleciona_doc     ,

* Seleciona LFA1
           z_seleciona_lfa1    ,

* Seleciona ZSDT0033
           z_seleciona_zsdt0033,

* Seleciona ZSDT_RETLOTE
           z_seleciona_retlote ,

* Seleciona DOC. Expor e NFE Export
         z_seleciona_doc_nfe_export,

* Seleciona ZSDT_EXPORT
           z_seleciona_zsdt_export,

* Seleciona VBFA
           z_seleciona_vbfa,

* Seleciona J_1BNFLIN
           z_seleciona_j_1bnflin,

* Seleciona DD07T
           z_seleciona_dd07t,

* Seleciona Notas Writer de Fom. de Lote
           z_seleciona_zfiwrt0008,

* Seleciona parceiros
           z_carta_correcao,

           z_seleciona_active,

           z_seleciona_zdoc_exp,

           z_seleciona_zsdt0053,

           z_seleciona_doc_ret,

           z_seleciona_doc_exp,

           z_selection_active_exp.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZSDT0002                                     *
*&---------------------------------------------------------------------*
*                             Seleciona ZSDT0002                       *
*----------------------------------------------------------------------*
FORM z_seleciona_zsdt0002.

  REFRESH t_zsdt0002.

  SELECT fkart
    FROM zsdt0002
    INTO TABLE t_zsdt0002
  WHERE  fkart NE space.

  SORT t_zsdt0002 BY fkart ASCENDING.
  CHECK t_zsdt0002[] IS INITIAL.
  CHECK it_zfiwrt0008[] IS NOT INITIAL.

  IF p_report NE 'ZSDR0123'.
    MESSAGE i836 WITH TEXT-002.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_ZSDT0002

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIKP                                         *
*&---------------------------------------------------------------------*
*                            Seleciona LIKP                            *
*----------------------------------------------------------------------*
FORM z_seleciona_likp.

  REFRESH t_likp.

  CHECK NOT t_zsdt0002[] IS INITIAL.

  SELECT vbeln erdat vstel
         vkorg kunnr fkarv
    FROM likp
    INTO TABLE t_likp
    FOR ALL ENTRIES IN t_zsdt0002
  WHERE  erdat IN s_erdat
    AND  vstel IN s_werks
    AND  vkorg IN s_bukrs
    AND  kunnr IN s_kunnr
    AND  fkarv EQ t_zsdt0002-fkart.

  SORT t_likp BY vbeln ASCENDING.
  CHECK t_likp[] IS INITIAL.
  CHECK it_zfiwrt0008[] IS INITIAL.

  IF p_report NE 'ZSDR0123'.
    MESSAGE i836 WITH TEXT-003.
    LEAVE LIST-PROCESSING.
  ENDIF.


ENDFORM.                    " Z_SELECIONA_LIKP

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIPS                                         *
*&---------------------------------------------------------------------*
*                           Seleciona LIPS                             *
*----------------------------------------------------------------------*
FORM z_seleciona_lips.

  REFRESH t_lips.

  CHECK NOT t_likp[] IS INITIAL.

  SELECT vbeln posnr matnr lgort vgbel
    FROM lips
    INTO TABLE t_lips
    FOR ALL ENTRIES IN t_likp
  WHERE  vbeln EQ t_likp-vbeln
    AND  matnr IN s_matnr.

  SORT t_lips BY vbeln ASCENDING
                 posnr ASCENDING.
  CHECK t_lips[] IS INITIAL.
  CHECK it_zfiwrt0008[] IS INITIAL.

  IF p_report NE 'ZSDR0123'.
    MESSAGE i836 WITH TEXT-003.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_LIPS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBRP                                         *
*&---------------------------------------------------------------------*
*                               Seleciona VBRP                         *
*----------------------------------------------------------------------*
FORM z_seleciona_vbrp.

  REFRESH t_vbrp.

  CHECK NOT t_lips[] IS INITIAL.

  SELECT vbeln posnr vgbel
         vgpos vgtyp
    FROM vbrp
    INTO TABLE t_vbrp
    FOR ALL ENTRIES IN t_lips
  WHERE  vgbel EQ t_lips-vbeln
    AND  vgpos EQ t_lips-posnr
    AND  vgtyp EQ c_j AND draft = space .

  SORT t_vbrp BY vgbel ASCENDING
                 vgpos ASCENDING.

* Move VBELN p/ REFKEY
  PERFORM z_move_vbeln_refkey.

  CHECK t_vbrp[] IS INITIAL.
  CHECK it_zfiwrt0008[] IS INITIAL.

  IF p_report NE 'ZSDR0123'.
    MESSAGE i836 WITH TEXT-004.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_VBRP

*&---------------------------------------------------------------------*
*&      Form  Z_MOVE_VBELN_REFKEY                                      *
*&---------------------------------------------------------------------*
*                          Move VBELN p/ REFKEY                        *
*----------------------------------------------------------------------*
FORM z_move_vbeln_refkey.

  DATA: sl_vbrp  TYPE type_vbrp,
        vl_index TYPE i.

  LOOP AT t_vbrp INTO sl_vbrp.

    vl_index = sy-tabix.

    sl_vbrp-refkey = sl_vbrp-vbeln.
    MODIFY t_vbrp FROM sl_vbrp INDEX vl_index
      TRANSPORTING refkey.

    CLEAR: sl_vbrp ,
           vl_index.

  ENDLOOP.

ENDFORM.                    " Z_MOVE_VBELN_REFKEY

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIN                                          *
*&---------------------------------------------------------------------*
*                         Seleciona J_1BNFLIN                          *
*----------------------------------------------------------------------*
FORM z_seleciona_lin.

  REFRESH t_lin.

  CHECK NOT t_vbrp[] IS INITIAL.

  SELECT docnum itmnum matnr  maktx
         refkey refitm reftyp menge charg nbm
    FROM j_1bnflin
    INTO TABLE t_lin
    FOR ALL ENTRIES IN t_vbrp
  WHERE  refkey EQ t_vbrp-refkey
    AND  refitm EQ t_vbrp-posnr
    AND  reftyp EQ c_bi
    AND  charg  IN s_charg.



  SORT: t_lin BY refkey ASCENDING
                 refitm ASCENDING.

  CHECK t_lin[] IS INITIAL.
  CHECK it_zfiwrt0008[] IS INITIAL.

  IF p_report NE 'ZSDR0123'.
    MESSAGE i836 WITH TEXT-005.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_LIN

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DOC                                          *
*&---------------------------------------------------------------------*
*                           Seleciona J_1BNFDOC                        *
*----------------------------------------------------------------------*
FORM z_seleciona_doc.

  DATA: tl_lin TYPE TABLE OF type_lin,
        wa_doc TYPE type_doc,
        wa_nad TYPE ty_j_1bnfnad.

  REFRESH t_doc.

  IF s_chave[] IS INITIAL AND s_matkl[] IS INITIAL.

    CHECK NOT t_lin[] IS INITIAL.
    tl_lin[] = t_lin[].
    SORT tl_lin BY docnum ASCENDING.
    DELETE ADJACENT DUPLICATES FROM tl_lin COMPARING docnum.

    IF s_docnum IS NOT INITIAL AND tl_lin IS NOT INITIAL.
      DELETE tl_lin WHERE docnum NOT IN s_docnum.
    ENDIF.

    SELECT docnum docdat bukrs
           branch parid  series
           nfnum  nfenum
      FROM j_1bnfdoc
      INTO TABLE t_doc
       FOR ALL ENTRIES IN tl_lin
     WHERE docnum EQ tl_lin-docnum
       AND cancel <> 'X'
       AND nftype <> 'ZJ'.
  ELSE.

    IF s_matkl[] IS INITIAL AND s_chave[] IS NOT INITIAL.
      "Chave de Acesso / 51 24 03 84590892000622 57000000742035110187282 2
      LOOP AT s_chave ASSIGNING FIELD-SYMBOL(<chave>).
        APPEND  VALUE #(
          regio   = <chave>-low+0(2)
          nfyear  = <chave>-low+2(2)
          nfmonth = <chave>-low+4(2)
          stcd1   = <chave>-low+6(14)
          model   = <chave>-low+20(2)
          serie   = <chave>-low+22(3)
          nfnum9  = <chave>-low+25(9)
          docnum9 = <chave>-low+34(9)
          cdv     = <chave>-low+43(1)
         ) TO it_nfe.
      ENDLOOP.

      "j_1bnfe_active
      FREE: it_active.
      SELECT *
      FROM j_1bnfe_active
      INTO TABLE it_active
      FOR ALL ENTRIES IN it_nfe
      WHERE    regio   EQ it_nfe-regio
         AND   nfyear  EQ it_nfe-nfyear
         AND   nfmonth EQ it_nfe-nfmonth
         AND   stcd1   EQ it_nfe-stcd1
         AND   model   EQ it_nfe-model
         AND   serie   EQ it_nfe-serie
         AND   nfnum9  EQ it_nfe-nfnum9.
    ELSE.
      "Pesquisar por grupo de mercadoria.
      IF s_matkl[] IS NOT INITIAL.
        SELECT docnum itmnum matnr  maktx
            refkey refitm reftyp menge charg nbm
            FROM j_1bnflin
            INTO TABLE t_lin
          WHERE matkl IN s_matkl
*            AND bukrs IN s_bukrs
            AND werks IN s_werks.

        IF sy-subrc EQ 0.
          FREE: it_active.
          SELECT *
          FROM j_1bnfe_active
          INTO TABLE it_active
          FOR ALL ENTRIES IN t_lin
          WHERE docnum EQ t_lin-docnum
             AND bukrs  IN s_bukrs
             AND branch IN s_werks.
        ENDIF.
      ENDIF.
    ENDIF.

    IF it_active IS NOT INITIAL.
      SELECT docnum docdat bukrs
                 branch parid  series
                 nfnum  nfenum
            FROM j_1bnfdoc
            INTO TABLE t_doc
             FOR ALL ENTRIES IN it_active
           WHERE docnum EQ it_active-docnum
             AND cancel <> 'X'
             AND nftype <> 'ZJ'.


      IF t_doc IS NOT INITIAL.
        SELECT docnum itmnum matnr  maktx
             refkey refitm reftyp menge charg nbm
        FROM j_1bnflin
        INTO TABLE t_lin
        FOR ALL ENTRIES IN t_doc
      WHERE  docnum EQ t_doc-docnum.

        IF t_lin IS NOT INITIAL.
          SORT: t_lin BY refkey ASCENDING
                          refitm ASCENDING.

          it_ov = VALUE #( FOR i IN t_lin ( vbeln = i-refkey posnr = i-refitm ) ).

          SELECT vbeln posnr vgbel vgpos vgtyp
          FROM vbrp
          INTO CORRESPONDING FIELDS OF TABLE t_vbrp
          FOR ALL ENTRIES IN it_ov
          WHERE  vbeln EQ it_ov-vbeln
          AND  posnr EQ it_ov-posnr
          AND  vgtyp EQ c_j AND draft = space .

          IF t_vbrp IS NOT INITIAL.
            SORT t_vbrp BY vgbel ASCENDING
                           vgpos ASCENDING.

            SELECT vbeln posnr matnr lgort vgbel
            FROM lips
            INTO TABLE t_lips
            FOR ALL ENTRIES IN t_vbrp
            WHERE  vbeln EQ t_vbrp-vgbel
            AND  posnr  EQ t_vbrp-vgpos.

            IF t_lips IS NOT INITIAL.
              DATA: r_fkart TYPE RANGE OF fkart.

              SORT t_lips BY vbeln ASCENDING
                             posnr ASCENDING.

              SELECT fkart
              FROM zsdt0002
              INTO TABLE t_zsdt0002
              WHERE  fkart NE space.
              r_fkart = VALUE #( FOR t IN t_zsdt0002 ( sign = 'I' option = 'EQ' low = t-fkart ) ).


              SELECT vbeln erdat vstel vkorg kunnr fkarv
              FROM likp
              INTO TABLE t_likp
              FOR ALL ENTRIES IN t_lips
              WHERE vbeln EQ t_lips-vbeln
                AND fkarv IN r_fkart.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT parid name1 docnum parvw
    FROM j_1bnfnad
    INTO TABLE t_j_1bnfnad
    FOR ALL ENTRIES IN t_doc
  WHERE docnum EQ t_doc-docnum
    AND parid IN s_parid
    AND parvw EQ 'Z1'.


  IF NOT ( s_parid IS INITIAL ).
    LOOP AT t_doc INTO wa_doc.

      READ TABLE t_j_1bnfnad INTO wa_nad WITH KEY docnum = wa_doc-docnum
                                                  parvw  = 'Z1'.

      IF ( sy-subrc EQ 0 ).
        CONTINUE.
      ELSE.
        DELETE t_doc WHERE docnum EQ wa_doc-docnum.
      ENDIF.

    ENDLOOP.
  ENDIF.

  CLEAR: wa_doc,
         wa_nad.

  SORT: t_doc       BY docnum ASCENDING,
        t_j_1bnfnad BY docnum ASCENDING.

ENDFORM.                    " Z_SELECIONA_DOC

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LFA1                                         *
*&---------------------------------------------------------------------*
*                             Seleciona LFA1                           *
*----------------------------------------------------------------------*
FORM z_seleciona_lfa1.

  DATA tl_doc TYPE TABLE OF type_doc.

  REFRESH t_lfa1.

  CHECK NOT t_doc[] IS INITIAL.
  tl_doc[] = t_doc[].

  SORT tl_doc BY parid ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_doc COMPARING parid.

  SELECT lifnr land1 name1
         regio
    FROM lfa1
    INTO TABLE t_lfa1
    FOR ALL ENTRIES IN tl_doc
  WHERE  lifnr EQ tl_doc-parid.

  SORT t_lfa1 BY lifnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_LFA1

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_RETLOTE                                      *
*&---------------------------------------------------------------------*
*                         Seleciona ZSDT_RETLOTE                       *
*----------------------------------------------------------------------*
FORM z_seleciona_retlote.

  REFRESH t_retlote.

  CHECK NOT t_doc[] IS INITIAL.

  SELECT docnum     nfenum     werks
         nf_retorno docnum_ret quant_vinc data_criacao id_export
    FROM zsdt_retlote
    APPENDING TABLE t_retlote
    FOR ALL ENTRIES IN t_doc
  WHERE  docnum EQ t_doc-docnum.

  SORT t_retlote BY docnum ASCENDING.

* Soma Quantidades
  PERFORM z_soma_qtd.

ENDFORM.                    " Z_SELECIONA_RETLOTE

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZSDT0033                                     *
*&---------------------------------------------------------------------*
*                           Seleciona ZSDT0033                         *
*----------------------------------------------------------------------*
FORM z_seleciona_zsdt0033.

  REFRESH t_zsdt0033.

  SELECT *
    FROM zsdt0033
    INTO TABLE t_zsdt0033
  WHERE land1 NE space.

  SORT t_zsdt0033 BY land1 ASCENDING
                     regio ASCENDING
                     matnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_ZSDT0033

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA_DADOS                                         *
*&---------------------------------------------------------------------*
*                              Processa Dados                          *
*----------------------------------------------------------------------*
FORM z_processa_dados.

  DATA: sl_doc        TYPE type_doc,
        sl_doc2       TYPE type_doc,
        sl_active     TYPE j_1bnfe_active,
        sl_lin        TYPE type_lin,
        sl_lfa1       TYPE type_lfa1,
        sl_sumret     TYPE type_retlote,
        sl_retlote    TYPE type_retlote,
        sl_export     TYPE ty_zsdt_export, "CSB 01.08.2022
        sl_doc_export TYPE ty_doc_export,
        sl_nfe_export TYPE ty_nfe_export,
        sl_vbfa       TYPE type_vbfa,
        sl_lin2       TYPE type_lin,
        sl_saida      TYPE type_saida,
        sl_dd07t      TYPE dd07t,
        sl_j_1bnfnad  TYPE ty_j_1bnfnad,
        index         TYPE sy-index,
        retlot        TYPE c LENGTH 1,
        index_doc     TYPE sy-index,
        wa_lfa1       TYPE lfa1,
        lva_quant     TYPE zsdt_export-quant.

  DATA: wl_zlest0146        TYPE zlest0146,
        lt_zlest0147        TYPE zlest0147_t,
        lt_zlest0168        TYPE zlest0168_t,
        v_doc_rateio        TYPE char01,
        lt_zib_nfe_dist_itm TYPE TABLE OF zib_nfe_dist_itm.

  SORT: it_zfiwrt0008 BY docnum,
        it_zfiwrt0015 BY seq_lcto.

  REFRESH t_saida.

  LOOP AT t_doc INTO sl_doc.

    CLEAR: sl_active.

    READ TABLE t_active INTO sl_active WITH KEY docnum = sl_doc-docnum.
    IF sy-subrc EQ 0.
      CONCATENATE sl_active-regio   "Região do emissor NF-e
                  sl_active-nfyear  "Ano da data do documento da NF-e
                  sl_active-nfmonth "Mês da data do documento da NF-e
                  sl_active-stcd1   "Nº CNPJ do emissor da NF-e
                  sl_active-model   "Modelo da nota fiscal
                  sl_active-serie   "SERIE
                  sl_active-nfnum9  "Nº NF-e de nove posições
                  sl_active-docnum9 "NF-e: nº aleatório
                  sl_active-cdv     "Dígito controle p/chave de acesso NF-e
             INTO sl_saida-chave_nfe.
    ENDIF.

    READ TABLE t_lfa1 INTO sl_lfa1
      WITH KEY lifnr = sl_doc-parid
      BINARY SEARCH.

    READ TABLE t_sumret INTO sl_sumret
      WITH KEY docnum = sl_doc-docnum
      BINARY SEARCH.

*    READ TABLE T_RETLOTE INTO SL_RETLOTE
*      WITH KEY DOCNUM = SL_DOC-DOCNUM
*      BINARY SEARCH.
*
*    READ TABLE T_EXPORT INTO SL_EXPORT
*      WITH KEY DOCNUM = SL_RETLOTE-DOCNUM_RET
*      BINARY SEARCH.
*
*    READ TABLE T_VBFA INTO SL_VBFA
*      WITH KEY VBELV = SL_EXPORT-ORDEM
*      BINARY SEARCH.
*
*    READ TABLE T_LIN2 INTO SL_LIN2
*      WITH KEY REFKEY = SL_VBFA-REFKEY
*      BINARY SEARCH.
*
*    READ TABLE T_DD07T INTO SL_DD07T
*      WITH KEY DOMVALUE_L = SL_EXPORT-FINALIDADE
*      BINARY SEARCH.




*   IDENTIFICA PARCEIRO EM CARTA DE CORREÇÃO
    READ TABLE it_zcarta INTO wa_zcarta WITH KEY docnum = sl_doc-docnum BINARY SEARCH.
    IF sy-subrc IS INITIAL.
*      READ TABLE IT_PARCE_CAR INTO WA_PARCE_CAR WITH KEY DOCNUM = SL_DOC-DOCNUM.
*      SL_SAIDA-NAME1_NAD = WA_PARCE_CAR-NAME1.
*      SL_SAIDA-PARID_NAD = WA_PARCE_CAR-PARID.
      READ TABLE t_lfa1 INTO DATA(sl_lfa1_z1)
          WITH KEY lifnr = wa_zcarta-novo_terminal
          BINARY SEARCH.

      IF sy-subrc EQ 0.
        sl_saida-name1_nad = sl_lfa1_z1-name1.
        sl_saida-parid_nad = sl_lfa1_z1-lifnr.
      ENDIF.
    ELSE.
*   PARCEIRO PADRAO
      READ TABLE t_j_1bnfnad INTO sl_j_1bnfnad WITH KEY docnum = sl_doc-docnum
                                                        parvw  = 'Z1'.
      IF sy-subrc EQ 0.
        sl_saida-name1_nad = sl_j_1bnfnad-name1.
        sl_saida-parid_nad = sl_j_1bnfnad-parid.
      ENDIF.
*   PARCEIRO NOTA WRITER
      READ TABLE  it_zfiwrt0008 INTO wa_zfiwrt0008 WITH KEY docnum = sl_doc-docnum BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE it_zfiwrt0015 WITH KEY seq_lcto = wa_zfiwrt0008-seq_lcto BINARY SEARCH.
        IF sy-subrc = 0.
          SELECT SINGLE *
            FROM lfa1
            INTO wa_lfa1
           WHERE lifnr =  it_zfiwrt0015-parid.
          sl_saida-name1_nad = wa_lfa1-name1.
          sl_saida-parid_nad = it_zfiwrt0015-parid.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE t_lin INTO sl_lin WITH KEY docnum = sl_doc-docnum.

    sl_saida-docnum  = sl_doc-docnum.
    sl_saida-itmnum  = sl_lin-itmnum.
    sl_saida-bukrs   = sl_doc-bukrs.
    sl_saida-branch  = sl_doc-branch.
    sl_saida-series  = sl_doc-series.
    sl_saida-docdat  = sl_doc-docdat.
    sl_saida-parid   = sl_doc-parid.
    sl_saida-name1   = sl_lfa1-name1.
    sl_saida-regio   = sl_lfa1-regio.
    sl_saida-matnr   = sl_lin-matnr.
    sl_saida-maktx   = sl_lin-maktx.
    sl_saida-menge   = sl_lin-menge.
    sl_saida-nbm     = sl_lin-nbm.

    CASE sl_lin-reftyp.
      WHEN 'BI'.
        READ TABLE t_vbrp INTO DATA(wl_vbrp) WITH KEY vbeln = sl_lin-refkey(10).
        IF sy-subrc EQ 0.
          READ TABLE t_lips INTO DATA(wl_lips) WITH KEY vbeln = wl_vbrp-vgbel.
          IF sy-subrc EQ 0.
            sl_saida-lgort   = wl_lips-lgort.
            sl_saida-remessa = wl_lips-vbeln.
            sl_saida-vgbel   = wl_lips-vgbel."DEVK9A22B4 - SD - ZSDT0034 - Incluir Coluna no ALV #143771 RSA
          ENDIF.
        ENDIF.
      WHEN 'ZW'.
        READ TABLE  it_zfiwrt0008 INTO wa_zfiwrt0008 WITH KEY docnum = sl_doc-docnum BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE  it_zfiwrt0009 INTO DATA(wa_zfiwrt0009) WITH KEY seq_lcto = wa_zfiwrt0008-seq_lcto.
          IF sy-subrc EQ 0.
            sl_saida-lgort = wa_zfiwrt0009-lgort.
          ENDIF.
        ENDIF.
    ENDCASE.

    "Check se Documento está registrado no CCT
    CLEAR: sl_saida-peso_aferido, sl_saida-dt_recepcao, sl_saida-dif_peso_cct, sl_saida-peso_fiscal_cct.

    CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
      EXPORTING
        i_docnum     = sl_saida-docnum
        i_itmnum     = sl_saida-itmnum
      IMPORTING
        e_zlest0146  = wl_zlest0146
        e_zlest0147  = lt_zlest0147
        e_zlest0168  = lt_zlest0168
        e_doc_rateio = v_doc_rateio.

    IF ( wl_zlest0146 IS NOT INITIAL ) AND ( v_doc_rateio IS INITIAL ).
      sl_saida-peso_aferido = wl_zlest0146-peso_aferido_recepcao.
      sl_saida-dt_recepcao  = wl_zlest0146-dt_recepcao.
      sl_saida-dif_peso_cct = sl_lin-menge - sl_saida-peso_aferido.

      READ TABLE lt_zlest0147 INTO DATA(wl_0147) INDEX 1.
      IF sy-subrc EQ 0.
        sl_saida-peso_fiscal_cct = wl_0147-peso_fiscal.
      ENDIF.

    ENDIF.

    sl_saida-saldo_ico   = sl_lin-menge - sl_sumret-quant_vinc.

    sl_saida-dias    = sy-datum - sl_saida-docdat.
*    SL_SAIDA-DOC_EXP = SL_LIN2-DOCNUM.
*    SL_SAIDA-QUANT   = SL_EXPORT-QUANT.
    sl_saida-charg   = sl_lin-charg.



*    CONCATENATE SL_EXPORT-FINALIDADE
*                SL_DD07T-DDTEXT
*           INTO SL_SAIDA-FINAL SEPARATED BY '-'.

    IF NOT sl_doc-nfenum IS INITIAL.
      sl_saida-nfenum = sl_doc-nfenum.
    ELSEIF NOT sl_doc-nfnum IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = sl_doc-nfnum
        IMPORTING
          output = sl_saida-nfenum.
    ENDIF.

*>>> SD - Remover data de recepção #IR200286 - GGARAUJO1 - 26/09/2024 - Início
      IF sl_saida-nfenum = '000077856' OR sl_saida-nfenum = '000077859'.
        CLEAR sl_saida-dt_recepcao.
      ENDIF.
*>>> SD - Remover data de recepção #IR200286 - GGARAUJO1 - 26/09/2024 - Fim

*     Atribui Icone
    PERFORM z_atribui_icone USING sl_lfa1
                                  sl_lin-matnr
                                  sl_saida-saldo_ico
                                  sl_doc-docdat
                         CHANGING sl_saida-icon.

**** RETLOTE WSB INICIO
****Conta quantos arquivos de Lotes o documento tem
    LOOP AT t_retlote INTO sl_retlote WHERE docnum EQ sl_doc-docnum.
      index = index + 1.
    ENDLOOP.

    LOOP AT t_retlote INTO sl_retlote WHERE docnum EQ sl_doc-docnum.
      index_doc = sy-tabix.
      retlot = 'X'.
****** no Loop retlote o index é comparado ao numero de registro da quantidade de
****** DocRetorno para ser impresso a Diferença somente no ultimo Documento.
      IF index EQ index_doc.
        sl_saida-menge = sl_lin-menge.
        sl_saida-saldo = sl_lin-menge - sl_sumret-quant_vinc.  "Original
        sl_saida-saldo_psaf_psfi = sl_sumret-quant_vinc - sl_saida-peso_aferido.
      ELSE.
        sl_saida-saldo = 0.
        sl_saida-menge = 0.
      ENDIF.

      sl_saida-doc_ret = sl_retlote-docnum_ret.
      sl_saida-q_vinc  = sl_retlote-quant_vinc.

      IF NOT sl_saida-doc_ret IS INITIAL.
        READ TABLE t_doc_ret INTO sl_doc_ret
        WITH KEY docnum = sl_saida-doc_ret.
        IF sy-subrc EQ 0.
          sl_saida-docdatret = sl_doc_ret-docdat.
        ENDIF.
      ENDIF.

***&&&&CS2016000807 INICIO>>>>>>

      READ TABLE t_export  INTO sl_export  WITH KEY docnum     = sl_retlote-docnum_ret id_export = sl_retlote-id_export   BINARY SEARCH.
      READ TABLE t_vbfa    INTO sl_vbfa    WITH KEY vbelv      = sl_export-ordem        BINARY SEARCH.
      READ TABLE t_lin2    INTO sl_lin2    WITH KEY refkey     = sl_vbfa-refkey         BINARY SEARCH.
      READ TABLE t_doc2    INTO sl_doc2    WITH KEY docnum     = sl_lin2-docnum         BINARY SEARCH.
      READ TABLE t_dd07t   INTO sl_dd07t   WITH KEY domvalue_l = sl_export-finalidade   BINARY SEARCH.

      sl_saida-doc_exp = sl_lin2-docnum.
      sl_saida-quant   = sl_export-quant.

** US - 81360 - Inicio - CBRAND
      IF sl_export-redex = 'X'.
        sl_saida-redex  = 'S'.
        READ TABLE t_export  INTO sl_export  WITH KEY docnum     = sl_retlote-docnum_ret id_export = sl_retlote-id_export   BINARY SEARCH.
        sl_saida-id_redex = sl_export-id_export.
        sl_saida-quant   = sl_export-quant.

      ELSE.
        sl_saida-redex  = 'N'.
      ENDIF.
** US - 81360 - fim - CBRAND

      IF NOT sl_saida-doc_exp IS INITIAL.
        READ TABLE t_doc_exp INTO sl_doc_exp
        WITH KEY docnum = sl_saida-doc_exp.
        IF sy-subrc EQ 0.
          sl_saida-docdatexp = sl_doc_exp-docdat.
        ENDIF.

        IF sl_saida-doc_exp IS NOT INITIAL.
          "Montar a chave documento de exportação.
          CLEAR: sl_activeexp.
          SELECT SINGLE * FROM j_1bnfe_active INTO sl_activeexp WHERE docnum EQ sl_saida-doc_exp.
          IF sy-subrc EQ 0.
            CONCATENATE sl_activeexp-regio   "Região do emissor NF-e
                             sl_activeexp-nfyear  "Ano da data do documento da NF-e
                             sl_activeexp-nfmonth "Mês da data do documento da NF-e
                             sl_activeexp-stcd1   "Nº CNPJ do emissor da NF-e
                             sl_activeexp-model   "Modelo da nota fiscal
                             sl_activeexp-serie   "SERIE
                             sl_activeexp-nfnum9  "Nº NF-e de nove posições
                             sl_activeexp-docnum9 "NF-e: nº aleatório
                             sl_activeexp-cdv     "Dígito controle p/chave de acesso NF-e
                        INTO sl_saida-chavenfeexp.
          ENDIF.
        ENDIF.
      ENDIF.

      "Montar a chave documento de retorno.

      IF sl_saida-doc_ret IS NOT INITIAL.
        CLEAR: sl_activeexp.
        SELECT SINGLE * FROM j_1bnfe_active INTO sl_activeexp WHERE docnum EQ sl_saida-doc_ret.
        IF sy-subrc EQ 0.
          CONCATENATE sl_activeexp-regio   "Região do emissor NF-e
                 sl_activeexp-nfyear  "Ano da data do documento da NF-e
                 sl_activeexp-nfmonth "Mês da data do documento da NF-e
                 sl_activeexp-stcd1   "Nº CNPJ do emissor da NF-e
                 sl_activeexp-model   "Modelo da nota fiscal
                 sl_activeexp-serie   "SERIE
                 sl_activeexp-nfnum9  "Nº NF-e de nove posições
                 sl_activeexp-docnum9 "NF-e: nº aleatório
                 sl_activeexp-cdv     "Dígito controle p/chave de acesso NF-e
            INTO sl_saida-chavedocret.
          SELECT SINGLE nfenum
            FROM j_1bnfdoc
            INTO sl_saida-nf_retorno
            WHERE docnum EQ sl_saida-doc_ret.
        ENDIF.
      ENDIF.

** US - 81360 - Inicio - CBRAND
      IF sl_saida-chavedocret IS NOT INITIAL AND sl_saida-redex  = 'S'.

        SELECT *
          FROM zib_nfe_dist_itm INTO
          TABLE lt_zib_nfe_dist_itm
         WHERE chave_nfe EQ sl_saida-chavedocret.

        IF lt_zib_nfe_dist_itm IS NOT INITIAL.

          CLEAR: lva_quant.
          LOOP AT t_export INTO DATA(w_zsdt_export) WHERE docnum  = sl_retlote-docnum_ret.
            lva_quant = w_zsdt_export-quant + lva_quant.
          ENDLOOP.

          CLEAR: sl_saida-sld_dev.
          READ TABLE lt_zib_nfe_dist_itm INTO DATA(lwa_zib_nfe_dist_itm) INDEX 1.
          sl_saida-sld_dev =  lva_quant - lwa_zib_nfe_dist_itm-prod_qtd_comerci.

        ENDIF.
      ENDIF.
** US - 81360 - Fim- CBRAND


      CONCATENATE sl_export-finalidade
                  sl_dd07t-ddtext
       INTO sl_saida-final SEPARATED BY '-'.

      IF sl_export-nf_retorno IS NOT INITIAL.
        sl_saida-nf_retorno = sl_export-nf_retorno.
      ENDIF.
      sl_saida-nfenumexp  = sl_doc2-nfenum.

      "aqui
      READ TABLE t_vbfaexp INTO sl_vbfaexp
      WITH KEY vbelv = sl_export-ordem.
      IF sy-subrc EQ 0.
        READ TABLE t_zdoc_exp INTO sl_zdoc_exp WITH KEY vbeln  = sl_vbfaexp-vbeln.
        IF sy-subrc EQ 0.
          READ TABLE t_zsdt0170 INTO sl_zsdt0170 WITH KEY id_due = sl_zdoc_exp-id_due.
          IF sy-subrc EQ 0.
            "sl_saida-id_due = sl_zdoc_exp-id_due. "ID
            "CHAVE DUE
            sl_saida-chave_acesso = sl_zsdt0170-chave_acesso.
            sl_saida-numero_due = sl_zsdt0170-numero_due. "Numero DUE
            IF sl_zsdt0170-situacao_due IS NOT INITIAL.
              PERFORM f_atrib_ds_dominio USING 'ZDM_SITUACAO_DUE'
                                        sl_zsdt0170-situacao_due
                               CHANGING sl_saida-ds_situacao_due.

            ENDIF.
            READ TABLE t_znom_transporte INTO DATA(wa_znom_transporte) WITH KEY id_nomeacao_tran = sl_zsdt0170-id_nomeacao_tran.
            IF sy-subrc EQ 0.
              sl_saida-ds_nome_transpor = wa_znom_transporte-ds_nome_transpor.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE t_doc_export INTO sl_doc_export  WITH KEY docnum_ret = sl_retlote-docnum_ret  BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE t_nfe_export  INTO sl_nfe_export  WITH KEY docnum = sl_doc_export-docnum  BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          sl_saida-nfenumexp  = sl_nfe_export-nfenum.
          sl_saida-doc_exp    = sl_nfe_export-docnum.

          SELECT SINGLE docdat
           FROM j_1bnfdoc
           INTO sl_saida-docdatexp
           WHERE docnum EQ sl_saida-doc_exp.

          IF sl_saida-doc_exp IS NOT INITIAL.
            "Montar a chave documento de Quebra.
            CLEAR: sl_activeexp.
            SELECT SINGLE * FROM j_1bnfe_active INTO sl_activeexp WHERE docnum EQ sl_saida-doc_exp.
            IF sy-subrc EQ 0.
              CONCATENATE sl_activeexp-regio   "Região do emissor NF-e
                               sl_activeexp-nfyear  "Ano da data do documento da NF-e
                               sl_activeexp-nfmonth "Mês da data do documento da NF-e
                               sl_activeexp-stcd1   "Nº CNPJ do emissor da NF-e
                               sl_activeexp-model   "Modelo da nota fiscal
                               sl_activeexp-serie   "SERIE
                               sl_activeexp-nfnum9  "Nº NF-e de nove posições
                               sl_activeexp-docnum9 "NF-e: nº aleatório
                               sl_activeexp-cdv     "Dígito controle p/chave de acesso NF-e
                          INTO sl_saida-chavenfeexp.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF s_final = 'Q' OR s_final = 'S' OR s_final = 'R'.
        IF sl_retlote-data_criacao IS NOT INITIAL.
          sl_saida-dias = sl_retlote-data_criacao - sl_saida-docdat.
        ELSE.
          sl_saida-dias = sy-datum - sl_saida-docdat.
        ENDIF.
      ENDIF.

      IF s_final IS NOT INITIAL AND sl_export-finalidade <> s_final.
        FREE: sl_doc2, sl_export, sl_vbfa, sl_lin2, sl_dd07t.
        CONTINUE.
      ENDIF.

      FREE: sl_doc2, sl_export, sl_vbfa, sl_lin2, sl_dd07t.

***&&&&CS2016000807 FIM<<<<<<<

      APPEND sl_saida TO t_saida.
      CLEAR: sl_saida-chave_acesso,sl_saida-numero_due,sl_saida-ds_situacao_due,sl_saida-docdatexp.

    ENDLOOP.

    READ TABLE t_retlote TRANSPORTING NO FIELDS WITH KEY docnum = sl_doc-docnum.
    IF sy-subrc IS NOT INITIAL.
      sl_saida-saldo = sl_lin-menge.
    ENDIF.

**** RETLOTE WSB FIM

    IF ( s_final IS INITIAL ) OR ( NOT s_final IS INITIAL AND s_final EQ sl_export-finalidade ).
      IF retlot IS INITIAL.

        READ TABLE t_doc_export INTO sl_doc_export  WITH KEY docnum_ret = sl_retlote-docnum_ret  BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE t_nfe_export  INTO sl_nfe_export  WITH KEY docnum = sl_doc_export-docnum  BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            sl_saida-nfenumexp  = sl_nfe_export-nfenum.
            sl_saida-doc_exp    = sl_nfe_export-docnum.
          ENDIF.
        ENDIF.

        IF s_final = 'Q' OR s_final = 'S' OR s_final = 'R'.
          IF sl_retlote-data_criacao IS NOT INITIAL.
            sl_saida-dias = sl_retlote-data_criacao - sl_saida-docdat.
          ELSE.
            sl_saida-dias = sy-datum - sl_saida-docdat.
          ENDIF.
        ENDIF.

        APPEND sl_saida TO t_saida.
      ENDIF.
    ENDIF.


    CLEAR: sl_lin, "Modificado por Welgem incluido fora do loop do retlote
     sl_saida.

    IF s_parid[] IS NOT INITIAL.
      DELETE t_saida WHERE parid_nad NOT IN s_parid.
    ENDIF.
    CLEAR: sl_doc,
           sl_lfa1,
           sl_sumret,
           sl_retlote,
           sl_export,
           sl_vbfa,
           sl_lin2,
           retlot,
           sl_dd07t,
           lt_zib_nfe_dist_itm,
           lwa_zib_nfe_dist_itm.

  ENDLOOP.

  PERFORM: z_consulta_notas_cct,
           z_atrib_conf_cct_portal.

ENDFORM.                    " Z_PROCESSA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SOMA_QTD                                               *
*&---------------------------------------------------------------------*
*                             Soma Quantidades                         *
*----------------------------------------------------------------------*
FORM z_soma_qtd.

  DATA: sl_retlote TYPE type_retlote,
        sl_sumret  TYPE type_retlote.

  REFRESH t_sumret.

  LOOP AT t_retlote INTO sl_retlote.
    sl_sumret-docnum     = sl_retlote-docnum.
    sl_sumret-quant_vinc = sl_retlote-quant_vinc.
    COLLECT sl_sumret INTO t_sumret.
    CLEAR: sl_retlote,
           sl_sumret .
  ENDLOOP.

ENDFORM.                    " Z_SOMA_QTD

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_FIELDCAT                                         *
*&---------------------------------------------------------------------*
*                           Monta FieldCat                             *
*----------------------------------------------------------------------*
FORM z_monta_fieldcat.

  REFRESH: t_fcat,
           t_tool.

* Preenche FieldCat
  PERFORM z_preenche_fieldcat USING:
    c_table 'ICON'      TEXT-018 04 space space,
    c_table 'BUKRS'     TEXT-006 07 space space,
    c_table 'BRANCH'    TEXT-007 06 space space,
    c_table 'DOCNUM'    TEXT-030 06 space space,
    c_table 'NFENUM'    TEXT-008 11 space c_x,
    c_table 'CHAVE_NFE'  TEXT-048 30 space space,

    c_table 'REMESSA'   TEXT-045 07 space space,
    c_table 'VGBEL'     TEXT-054 10 space space, "DEVK9A22B4 - SD - ZSDT0034 - Incluir Coluna no ALV #143771 RSA
    c_table 'CHARG'     TEXT-029 10 space space,

    c_table 'DOC_RET'   TEXT-023 12 space c_x,
    c_table 'ID_REDEX'   TEXT-052 12 space c_x,
    c_table 'NF_RETORNO' TEXT-031 12 space space,
    c_table 'CHAVEDOCRET' TEXT-046 30 space space,

    c_table 'DOC_EXP'   TEXT-024 15 space c_x,
    c_table 'NFENUMEXP' TEXT-032 15 space space,
    c_table 'CHAVENFEEXP' TEXT-047 30 space space,

    c_table 'FINAL'     TEXT-026 10 space space,
    c_table 'QUANT'     TEXT-025 17 space space,
    c_table 'REDEX'     TEXT-050 05 space space,
    c_table 'SLD_DEV'   TEXT-051 09 space space,
    c_table 'SERIES'    TEXT-009 05 space space,
    c_table 'DOCDAT'    TEXT-010 11 space space,
    c_table 'DIAS'      TEXT-022 08 space space,
    c_table 'PARID'     TEXT-011 10 space space,
    c_table 'NAME1'     TEXT-012 20 space space,
    c_table 'REGIO'     TEXT-013 06 space space,
    c_table 'MATNR'     TEXT-014 14 c_x   space,
    c_table 'MAKTX'     TEXT-015 20 space space,
    c_table 'NBM'       TEXT-036 20 space space,
    c_table 'MENGE'     TEXT-016 14 space space,
    c_table 'LGORT'     TEXT-042 14 space space,
    c_table 'Q_VINC'    TEXT-017 14 space space,
    c_table 'SALDO'     TEXT-021 14 space space,
    c_table 'PARID_NAD' TEXT-028 14 space space,
    c_table 'NAME1_NAD' TEXT-027 14 space space,
    c_table 'DT_RECEPCAO'      TEXT-033 19 space space,
    c_table 'PESO_AFERIDO'     TEXT-034 19 space space,
    c_table 'PESO_FISCAL_CCT'  TEXT-043 19 space space,
    c_table 'DIF_PESO_CCT' TEXT-035 19 space space,
    c_table 'CONF_CCT_PORTAL'    TEXT-038 15 space space,
    c_table 'DT_RECEPCAO_PORTAL' TEXT-041 13 space space,
    c_table 'TERM_CCT_PORTAL'    TEXT-039 12 space space,
    c_table 'DS_TERM_CCT_PORTAL' TEXT-040 30 space space,
    c_table 'SALDO_PSAF_PSFI' TEXT-044 30 space space,
    c_table 'NUMERO_DUE' 'Número DU-e' 30 space space,
    c_table 'DS_SITUACAO_DUE' TEXT-049 30 space space,
    c_table 'DS_NOME_TRANSPOR' TEXT-053 50 space space,

    c_table 'CHAVE_ACESSO' 'Chave Acesso DU-e' 30 space space,
    c_table 'DOCDATRET' 'Data Retorno' 30 space space,
    c_table 'DOCDATEXP' 'Data Exportação' 30 space space.


* Monta Layout
  PERFORM z_layout.

ENDFORM.                    " Z_MONTA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_FIELDCAT                                      *
*&---------------------------------------------------------------------*
*                           Preenche FieldCat                          *
*----------------------------------------------------------------------*
FORM z_preenche_fieldcat USING p_table TYPE c
                               p_field TYPE c
                               p_desc  TYPE c
                               p_len   TYPE n
                               p_zero  TYPE c
                               p_hot   TYPE c.

  DATA sl_fcat TYPE lvc_s_fcat.

  sl_fcat-tabname   = p_table.
  sl_fcat-fieldname = p_field.
  sl_fcat-scrtext_l = p_desc.
  sl_fcat-scrtext_m = p_desc.
  sl_fcat-scrtext_s = p_desc.
  sl_fcat-outputlen = p_len.
  sl_fcat-no_zero   = p_zero.
  sl_fcat-hotspot   = p_hot.

  IF p_field EQ 'CONF_CCT_PORTAL'.
    sl_fcat-just = 'C'.
  ENDIF.

  APPEND sl_fcat TO t_fcat.

ENDFORM.                    " Z_PREENCHE_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT                                                 *
*&---------------------------------------------------------------------*
*                            Monta Layout                              *
*----------------------------------------------------------------------*
FORM z_layout.

  CLEAR s_layout.

  s_layout-zebra      = 'X'.
  s_layout-cwidth_opt = 'X'.

ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      SET PF-STATUS 'PF0100'.
      SET TITLEBAR 'TB0100'.
  ENDCASE.

ENDMODULE.                 " ZM_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_OBJ_ALV  OUTPUT                                     *
*&---------------------------------------------------------------------*
*                                 Obj Alv                              *
*----------------------------------------------------------------------*
MODULE zm_obj_alv OUTPUT.

* Instancia Container
  PERFORM: z_inst_cont ,
* Instancia Alv
           z_inst_alv  ,
* Instancia Eventos
           z_inst_event,
* Exibe Alv
           z_exibe_alv .



ENDMODULE.                 " ZM_OBJ_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_CONT                                              *
*&---------------------------------------------------------------------*
*                       Instancia Container                            *
*----------------------------------------------------------------------*
FORM z_inst_cont.

  CHECK s_cont IS INITIAL.

  CREATE OBJECT s_cont
    EXPORTING
      container_name              = 'CC_ALV'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF NOT sy-subrc IS INITIAL.
    IF p_report NE 'ZSDR0123'.
      MESSAGE i836 WITH TEXT-019.
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_INST_CONT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_ALV                                               *
*&---------------------------------------------------------------------*
*                              Instancia Alv                           *
*----------------------------------------------------------------------*
FORM z_inst_alv.

  CHECK s_alv IS INITIAL.

  CREATE OBJECT s_alv
    EXPORTING
      i_parent          = s_cont
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  IF NOT sy-subrc IS INITIAL.
    IF p_report NE 'ZSDR0123'.
      MESSAGE i836 WITH TEXT-020.
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_INST_ALV


*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ALV                                              *
*&---------------------------------------------------------------------*
*                                Exibe Alv                             *
*----------------------------------------------------------------------*
FORM z_exibe_alv.

  DATA vl_int TYPE int4.

  gs_variant_c-variant = p_varia.
  s_layout-col_opt = 'X'.

  CALL METHOD s_alv->set_table_for_first_display
    EXPORTING
      i_default                     = c_x
      is_layout                     = s_layout
      is_variant                    = gs_variant_c
      i_save                        = 'A'
*     it_toolbar_excluding          = t_tool
    CHANGING
      it_outtab                     = t_saida
      it_fieldcatalog               = t_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

*  CALL METHOD s_alv->set_ready_for_input.

  IF NOT sy-subrc IS INITIAL.
    IF p_report NE 'ZSDR0123'.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_EXIBE_ALV

*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              Exit Command                            *
*----------------------------------------------------------------------*
MODULE zm_exit_command INPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      CASE sy-ucomm.
        WHEN 'BACK' OR
             'CANC' OR
             'EXIT'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_EXIT_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              User Command                            *
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_ATRIBUI_ICONE                                          *
*&---------------------------------------------------------------------*
*                               Atribui Icone                          *
*----------------------------------------------------------------------*
FORM z_atribui_icone USING p_lfa1  TYPE type_lfa1
                           p_matnr TYPE mara-matnr
                           p_saldo TYPE ccm_quant
                           p_data  TYPE j_1bnfdoc-docdat
                  CHANGING p_icon  TYPE icon-name.

  DATA: sl_zsdt0033 TYPE zsdt0033,
        vl_alerta   TYPE datum,
        vl_critico  TYPE datum.

  CLEAR p_icon.

  READ TABLE t_zsdt0033 INTO sl_zsdt0033
    WITH KEY land1 = p_lfa1-land1
             regio = p_lfa1-regio
             matnr = p_matnr
    BINARY SEARCH.

  IF sy-subrc IS INITIAL.

    vl_alerta  = p_data + sl_zsdt0033-critico - sl_zsdt0033-alerta.
    vl_critico = p_data + sl_zsdt0033-critico.

    IF p_saldo GT 0 AND ( sy-datum GE vl_alerta AND sy-datum LE vl_critico ).
      p_icon = icon_status_alert.
      EXIT.
    ENDIF.

    IF p_saldo GT 0 AND sy-datum LT vl_alerta.
      p_icon = icon_status_ok.
      EXIT.
    ENDIF.

    IF p_saldo LE 0.
      p_icon = icon_status_best.
      EXIT.
    ENDIF.

    IF p_saldo GT 0 AND sy-datum GT vl_critico.
      p_icon = icon_status_critical.
      EXIT.
    ENDIF.

  ENDIF.

  IF p_icon IS INITIAL.
    p_icon = icon_failure.
  ENDIF.

ENDFORM.                    " Z_ATRIBUI_ICONE

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT                                         *
*&---------------------------------------------------------------------*
*                                HotSpot                               *
*----------------------------------------------------------------------*
FORM z_handle_hotspot USING p_row_id    TYPE lvc_s_row
                            p_column_id TYPE lvc_s_col
                            p_row_no    TYPE lvc_s_roid.

  IF p_column_id-fieldname EQ c_nfenum.
    PERFORM z_chama_j1b3n USING p_row_id-index
                                'NFE'.
  ENDIF.

  IF p_column_id-fieldname EQ c_docret.
    PERFORM z_chama_j1b3n USING p_row_id-index
                                'RET'.
  ENDIF.

  IF p_column_id-fieldname EQ c_docexp.
    PERFORM z_chama_j1b3n USING p_row_id-index
                                'EXP'.
  ENDIF.

ENDFORM.                    " Z_HANDLE_HOTSPOT

*&---------------------------------------------------------------------*
*&      Form  Z_CHAMA_J1B3N                                            *
*&---------------------------------------------------------------------*
*                             Chama J1B3N                              *
*----------------------------------------------------------------------*
FORM z_chama_j1b3n USING p_index TYPE lvc_index
                         p_ref   TYPE c.

  DATA: sl_saida  TYPE type_saida,
        vl_nfobjn TYPE j_1binterf-nfobjn,
        vl_docnum TYPE j_1bnfdoc-docnum.

  CLEAR: vl_nfobjn,
         sl_saida .

  READ TABLE t_saida INTO sl_saida INDEX p_index.

  CASE p_ref.
    WHEN 'NFE'.
      vl_docnum = sl_saida-docnum.
    WHEN 'RET'.
      vl_docnum = sl_saida-doc_ret.
    WHEN 'EXP'.
      vl_docnum = sl_saida-doc_exp.
  ENDCASE.

  CHECK sy-subrc  IS INITIAL AND NOT
        vl_docnum IS INITIAL.

  CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
    EXPORTING
      doc_number         = vl_docnum
    IMPORTING
      obj_number         = vl_nfobjn
    EXCEPTIONS
      document_not_found = 1
      docum_lock         = 2
      OTHERS             = 3.

  CHECK NOT vl_nfobjn IS INITIAL.

  CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
    EXPORTING
      obj_number         = vl_nfobjn
    EXCEPTIONS
      object_not_found   = 1
      scr_ctrl_not_found = 2
      OTHERS             = 3.

ENDFORM.                    " Z_CHAMA_J1B3N

*&---------------------------------------------------------------------*
*&      Form  Z_INST_EVENT                                             *
*&---------------------------------------------------------------------*
*                           Instancia Eventos                          *
*----------------------------------------------------------------------*
FORM z_inst_event.

  CHECK s_event IS INITIAL.

  CREATE OBJECT s_event.
  SET HANDLER s_event->zm_handle_hotspot FOR s_alv.

ENDFORM.                    " Z_INST_EVENT

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZSDT_EXPORT                                  *
*&---------------------------------------------------------------------*
*                         Seleciona ZSDT_EXPORT                        *
*----------------------------------------------------------------------*
FORM z_seleciona_zsdt_export.

  DATA tl_retlote TYPE TABLE OF type_retlote.

  REFRESH t_export.

  CHECK NOT t_retlote[] IS INITIAL.

  tl_retlote[] = t_retlote[].
  SORT tl_retlote BY docnum_ret ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_retlote COMPARING docnum_ret.
  DELETE tl_retlote WHERE docnum_ret IS INITIAL.

  CHECK NOT tl_retlote[] IS INITIAL.

*  SELECT  mandt
*          docnum
*          werks
*          nf_retorno
*          ordem
*          data_criacao
*          quant
*          valor_total
*          export
*          matnr
*          status
*          finalidade
*          id_export
*          redex
*    FROM zsdt_export
*     INTO CORRESPONDING FIELDS OF TABLE t_export
*    FOR ALL ENTRIES IN tl_retlote
*  WHERE  docnum EQ tl_retlote-docnum_ret.

  SELECT *
    FROM zsdt_export
    APPENDING TABLE t_export
    FOR ALL ENTRIES IN tl_retlote
  WHERE  docnum EQ tl_retlote-docnum_ret.

  SORT t_export BY docnum ASCENDING.

ENDFORM.                    " Z_SELECIONA_ZSDT_EXPORT

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBFA                                         *
*&---------------------------------------------------------------------*
*                            Seleciona VBFA                            *
*----------------------------------------------------------------------*
FORM z_seleciona_vbfa.

  DATA: tl_export TYPE TABLE OF zsdt_export,
        sl_vbfa   TYPE type_vbfa,
        vl_index  TYPE i.

  REFRESH t_vbfa.

  CHECK NOT t_export[] IS INITIAL.

  tl_export[] = t_export[].
  SORT tl_export BY ordem ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_export COMPARING ordem.
  DELETE tl_export WHERE ordem IS INITIAL.

  CHECK NOT tl_export[] IS INITIAL.

  SELECT vbelv  posnv   vbeln
         posnn  vbtyp_n vbtyp_v
    FROM vbfa AS a
    APPENDING TABLE t_vbfa
    FOR ALL ENTRIES IN tl_export
  WHERE  vbelv   EQ tl_export-ordem
    AND  vbtyp_n EQ 'M'
    AND  vbtyp_v EQ 'C'
    AND NOT EXISTS ( SELECT *
                       FROM vbfa AS b
                      WHERE b~vbelv   = a~vbeln
                        AND b~vbtyp_n = 'N' "estorno
                   ).

  SORT t_vbfa BY vbelv ASCENDING.

  LOOP AT t_vbfa INTO sl_vbfa.

    vl_index = sy-tabix.

    sl_vbfa-refkey = sl_vbfa-vbeln.

    MODIFY t_vbfa FROM sl_vbfa INDEX vl_index
      TRANSPORTING refkey.

    CLEAR sl_vbfa.

  ENDLOOP.

ENDFORM.                    " Z_SELECIONA_VBFA

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_J_1BNFDOC                                    *
*&---------------------------------------------------------------------*
*                            Seleciona J_1BNFDOC                       *
*----------------------------------------------------------------------*
FORM z_seleciona_j_1bnflin.

  DATA tl_vbfa TYPE TABLE OF type_vbfa.

  REFRESH t_lin2.

  CHECK NOT t_vbfa[] IS INITIAL.

  tl_vbfa[] = t_vbfa[].
  SORT tl_vbfa BY refkey ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_vbfa COMPARING refkey.
  DELETE tl_vbfa WHERE refkey IS INITIAL.

  CHECK NOT tl_vbfa[] IS INITIAL.

  SELECT docnum itmnum matnr  maktx
         refkey refitm reftyp menge
         nbm
    FROM j_1bnflin
    APPENDING TABLE t_lin2
    FOR ALL ENTRIES IN tl_vbfa
  WHERE  refkey EQ tl_vbfa-refkey.

  SELECT docnum docdat bukrs
         branch parid  series
         nfnum  nfenum
    FROM j_1bnfdoc
      INTO TABLE t_doc2
        FOR ALL ENTRIES IN t_lin2
    WHERE docnum EQ t_lin2-docnum.

  SORT: t_lin2 BY refkey ASCENDING,
        t_doc2 BY docnum ASCENDING.

ENDFORM.                    " Z_SELECIONA_J_1BNFLIN

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DD07T                                        *
*&---------------------------------------------------------------------*
*                             Seleciona DD07T                          *
*----------------------------------------------------------------------*
FORM z_seleciona_dd07t.

  REFRESH t_dd07t.

  SELECT *
    FROM dd07t
    INTO TABLE t_dd07t
  WHERE  domname    EQ 'ZFIN_EXPORT_D'
    AND  ddlanguage EQ sy-langu.

  SORT t_dd07t BY domvalue_l ASCENDING.

ENDFORM.                    " Z_SELECIONA_DD07T
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DOC_NFE_EXPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_doc_nfe_export.

  CLEAR: t_doc_export, t_nfe_export.

  IF t_retlote[] IS NOT INITIAL.

    SELECT docnum_retorno docnum
      FROM zfiwrt0008
      INTO TABLE t_doc_export
      FOR ALL ENTRIES IN t_retlote
      WHERE docnum_retorno = t_retlote-docnum_ret.

    IF sy-subrc IS INITIAL.

      SELECT docnum nfenum
        FROM j_1bnfdoc
        INTO TABLE t_nfe_export
        FOR ALL ENTRIES IN t_doc_export
        WHERE docnum = t_doc_export-docnum.

    ENDIF.
    SORT: t_doc_export BY docnum_ret,
          t_nfe_export BY docnum.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZFIWRT0008
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_zfiwrt0008 .

  IF s_chave[] IS INITIAL AND s_matkl[] IS INITIAL.
    CHECK it_zfiwrt0008[] IS NOT INITIAL.
    SELECT docnum docdat bukrs
       branch parid  series
       nfnum  nfenum
      FROM j_1bnfdoc
      APPENDING TABLE t_doc
      FOR ALL ENTRIES IN it_zfiwrt0008
      WHERE  docnum EQ it_zfiwrt0008-docnum
       AND cancel <> 'X'.

    SELECT parid name1 docnum parvw
      FROM j_1bnfnad
      APPENDING TABLE t_j_1bnfnad
      FOR ALL ENTRIES IN it_zfiwrt0008
    WHERE docnum EQ it_zfiwrt0008-docnum
      AND parid IN s_parid
      AND parvw EQ 'Z1'.

    IF it_zfiwrt0008[] IS NOT INITIAL.
      SELECT *
         INTO TABLE it_zfiwrt0015
         FROM zfiwrt0015
          FOR ALL ENTRIES IN it_zfiwrt0008
        WHERE seq_lcto EQ it_zfiwrt0008-seq_lcto
          AND parvw    EQ 'Z1'.
    ENDIF.

    SELECT docnum itmnum matnr  maktx
           refkey refitm reftyp menge charg nbm
      FROM j_1bnflin
      APPENDING TABLE t_lin
     FOR ALL ENTRIES IN it_zfiwrt0008
      WHERE  docnum EQ it_zfiwrt0008-docnum
      AND    matnr IN s_matnr
      AND  charg  IN s_charg.

    SELECT lifnr land1 name1
         regio
    FROM lfa1
    APPENDING TABLE t_lfa1
    FOR ALL ENTRIES IN it_zfiwrt0008
  WHERE  lifnr EQ it_zfiwrt0008-parid.
  ENDIF.



  PERFORM:
* Seleciona ZSDT_RETLOTE
         z_seleciona_retlote ,

* Seleciona ZSDT_EXPORT
         z_seleciona_zsdt_export,

* Seleciona VBFA
         z_seleciona_vbfa,

* Seleciona J_1BNFLIN
         z_seleciona_j_1bnflin.

  SORT: t_doc       BY docnum ASCENDING,
        t_j_1bnfnad BY docnum ASCENDING,
        t_lfa1      BY lifnr ASCENDING,
        t_lin       BY refkey ASCENDING
                       refitm ASCENDING.


ENDFORM.                    " Z_SELECIONA_ZFIWRT0008
*&---------------------------------------------------------------------*
*&      Form  Z_CARTA_CORRECAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_carta_correcao .
  REFRESH it_zcarta.

  IF it_zfiwrt0008[] IS NOT INITIAL.
    SELECT *
      FROM zcarta_correcao
      INTO CORRESPONDING FIELDS OF TABLE it_zcarta
      FOR ALL ENTRIES IN it_zfiwrt0008
      WHERE docnum        =  it_zfiwrt0008-docnum
       AND  novo_terminal <> ''.
  ENDIF.


  IF t_doc[] IS NOT INITIAL.
    SELECT *
      FROM zcarta_correcao
      APPENDING CORRESPONDING FIELDS OF TABLE it_zcarta
      FOR ALL ENTRIES IN t_doc
      WHERE docnum        =  t_doc-docnum
       AND  novo_terminal <> ''.
  ENDIF.

  SORT it_zcarta BY docnum  ASCENDING id_cc DESCENDING.

  IF it_zcarta[] IS NOT INITIAL.
    SELECT lifnr land1 name1
        regio
       FROM lfa1
       APPENDING TABLE t_lfa1
       FOR ALL ENTRIES IN it_zcarta
     WHERE  lifnr EQ it_zcarta-novo_terminal.

    SORT t_lfa1 BY lifnr ASCENDING.
*    SELECT *
*      FROM J_1BNFNAD
*      INTO CORRESPONDING FIELDS OF TABLE IT_PARCE_CAR
*      FOR ALL ENTRIES IN IT_ZCARTA
*      WHERE DOCNUM = IT_ZCARTA-DOCNUM.
  ENDIF.


ENDFORM.                    " Z_CARTA_CORRECAO

FORM z_seleciona_active.

  CHECK t_doc[] IS NOT INITIAL.

  SELECT *
    FROM j_1bnfe_active INTO TABLE t_active
    FOR ALL ENTRIES IN t_doc
  WHERE docnum = t_doc-docnum.

ENDFORM.


FORM z_consulta_notas_cct.

  DATA: it_nfe_cons TYPE zde_chave_doc_e_t,
        wl_nfe_cons TYPE zde_chave_doc_e,
        sl_saida    TYPE type_saida.

  CHECK p_srvcct EQ abap_true.

  CLEAR: it_nfe_cons[].

  LOOP AT t_saida INTO sl_saida
    WHERE chave_nfe IS NOT INITIAL
      AND conf_cct_portal IS INITIAL.

    wl_nfe_cons = sl_saida-chave_nfe.
    APPEND wl_nfe_cons TO it_nfe_cons.

  ENDLOOP.

  CHECK it_nfe_cons[] IS NOT INITIAL.

  CALL FUNCTION 'ZCCT_CONFIRMA_REC_NF_PORTAL'
    EXPORTING
      i_job    = p_report
      i_chaves = it_nfe_cons.

ENDFORM.

FORM z_atrib_conf_cct_portal.

  DATA: tg_zlest0186 TYPE TABLE OF zlest0186  WITH HEADER LINE,
        tg_lfa1      TYPE TABLE OF lfa1       WITH HEADER LINE,
        tg_zsdt0168  TYPE TABLE OF zsdt0168   WITH HEADER LINE.

  t_saida_aux[] = t_saida[].
  DELETE t_saida_aux WHERE chave_nfe IS INITIAL.

  IF t_saida_aux[] IS NOT INITIAL.
    SELECT *
      FROM zlest0186 APPENDING TABLE tg_zlest0186
       FOR ALL ENTRIES IN t_saida_aux
     WHERE chave = t_saida_aux-chave_nfe.
  ENDIF.

  SORT tg_zlest0186 BY chave.
  DELETE ADJACENT DUPLICATES FROM tg_zlest0186 COMPARING chave.

  CHECK tg_zlest0186[] IS NOT INITIAL.

  SELECT *
    FROM zsdt0168 APPENDING TABLE tg_zsdt0168
     FOR ALL ENTRIES IN tg_zlest0186
   WHERE codigo_ra EQ tg_zlest0186-codigo_ra.

  IF tg_zsdt0168[] IS NOT INITIAL.
    SELECT *
      FROM lfa1 APPENDING TABLE tg_lfa1
       FOR ALL ENTRIES IN tg_zsdt0168
     WHERE lifnr EQ tg_zsdt0168-lifnr.
  ENDIF.

  SORT tg_lfa1 BY lifnr.
  DELETE ADJACENT DUPLICATES FROM tg_lfa1 COMPARING lifnr.

  LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

    READ TABLE tg_zlest0186 WITH KEY chave = <fs_saida>-chave_nfe.

    IF sy-subrc EQ 0.
      <fs_saida>-conf_cct_portal    = icon_okay.
      <fs_saida>-dt_recepcao_portal = tg_zlest0186-dt_recepcao.

      READ TABLE tg_zsdt0168 WITH KEY codigo_ra = tg_zlest0186-codigo_ra.
      IF sy-subrc EQ 0.
        <fs_saida>-term_cct_portal   = tg_zsdt0168-lifnr.

        READ TABLE tg_lfa1 WITH KEY lifnr = tg_zsdt0168-lifnr.
        IF sy-subrc EQ 0.
          <fs_saida>-ds_term_cct_portal = tg_lfa1-name1.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZDOC_EXP
*&---------------------------------------------------------------------*
FORM z_seleciona_zdoc_exp .

  CLEAR t_vbfaexp.
  CHECK t_export[] IS NOT INITIAL.
  SELECT * INTO TABLE t_vbfaexp
      FROM vbfa
      FOR ALL ENTRIES IN t_export
    WHERE  vbelv   EQ t_export-ordem
      AND  vbtyp_n EQ 'J'
      AND  vbtyp_v EQ 'C'.

  IF t_vbfaexp[] IS NOT INITIAL.
    SELECT * INTO TABLE t_zdoc_exp
        FROM zdoc_exp AS e
        FOR ALL ENTRIES IN t_vbfaexp
        WHERE  vbeln  = t_vbfaexp-vbeln
          AND NOT EXISTS ( SELECT *
                            FROM zdoc_exp_recusa AS b
                           WHERE b~id_doc_exp = e~id_doc_exp ).


    IF t_zdoc_exp[] IS NOT INITIAL.
**    seleciona a tabela zsdt0170.
**    DU-e
      SELECT *
        FROM zsdt0170
        INTO TABLE t_zsdt0170
        FOR ALL ENTRIES IN t_zdoc_exp
        WHERE id_due EQ t_zdoc_exp-id_due.
**    "pega informação zsdt0170-numero da due
**    "pega informação zsdt0170-chave due

*133796 - Incluir a coluna nome do navio  ITSOUZA
      IF sy-subrc EQ 0.
        SELECT * FROM znom_transporte
          INTO TABLE t_znom_transporte
          FOR ALL ENTRIES IN t_zsdt0170
          WHERE id_nomeacao_tran = t_zsdt0170-id_nomeacao_tran.
      ENDIF.

    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZSDT0053
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_zsdt0053 .

  CHECK t_zdoc_exp IS NOT INITIAL.
  SELECT * INTO TABLE t_zsdt0053
      FROM zsdt0053
      FOR ALL ENTRIES IN t_zdoc_exp
      WHERE vbeln = t_zdoc_exp-vbeln.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DOC_RET
*&---------------------------------------------------------------------*
FORM z_seleciona_doc_ret.
  CHECK t_retlote[] IS NOT INITIAL.
  SELECT docnum docdat INTO TABLE t_doc_ret
      FROM j_1bnfdoc
      FOR ALL ENTRIES IN t_retlote
      WHERE docnum = t_retlote-docnum_ret.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DOC_EXP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_doc_exp .
  CHECK t_lin2[] IS NOT INITIAL.
  SELECT docnum docdat INTO TABLE t_doc_exp
      FROM j_1bnfdoc
      FOR ALL ENTRIES IN t_lin2
      WHERE docnum = t_lin2-docnum.



ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECTION_ACTIVE_EXP
*&---------------------------------------------------------------------*
FORM z_selection_active_exp .
  CHECK t_doc_ret[] IS NOT INITIAL.
  SELECT * INTO TABLE t_activeexp
      FROM j_1bnfe_active
      FOR ALL ENTRIES IN t_doc_ret
      WHERE docnum = t_doc_ret-docnum.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATRIB_DS_DOMINIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2742   text
*      -->P_WA_SAIDA_0100_SITUACAO_DUE  text
*      <--P_WA_SAIDA_0100_DS_SITUACAO_DUE  text
*----------------------------------------------------------------------*
FORM f_atrib_ds_dominio USING p_domname    TYPE dd07t-domname
                              p_domvalue_l
                     CHANGING c_ddtext     TYPE dd07t-ddtext.

  DATA: values   TYPE vrm_values WITH HEADER LINE,
        tg_dd07t TYPE TABLE OF dd07t WITH HEADER LINE.

  DATA: v_value TYPE dd07t-domvalue_l.

  CLEAR: values[], values, tg_dd07t[], c_ddtext.

  CHECK ( p_domname     IS NOT INITIAL ) AND
        ( p_domvalue_l  IS NOT INITIAL ).

  v_value = CONV #( p_domvalue_l ).

  SELECT SINGLE ddtext
    FROM dd07t INTO c_ddtext
   WHERE domname    = p_domname
     AND ddlanguage = sy-langu
     AND domvalue_l = v_value.

ENDFORM.
