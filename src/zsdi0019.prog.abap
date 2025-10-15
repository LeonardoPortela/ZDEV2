*----------------------------------------------------------------------*
*                   WAYON     C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDI0019                                                *
* Descrição  : Relatório de Controle Prazo R.Form.Lote                 *
* Módulo     : SD                                Transação: ZSDT0038   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Alexandre Suzan                        Data: 27/01/2021 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zsdi0019 NO STANDARD PAGE HEADING MESSAGE-ID sd.

*----------------------------------------------------------------------*
*                                Type Pools                            *
*----------------------------------------------------------------------*
TYPE-POOLS icon.

TABLES: t001 ,
        t001w,
        likp ,
        lips ,
        j_1bnfnad,
        zsdt_depara_depo,
        j_1bnflin.

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

       BEGIN OF type_doc,
         docnum TYPE j_1bnfdoc-docnum,
         docdat TYPE j_1bnfdoc-docdat,
         bukrs  TYPE j_1bnfdoc-bukrs,
         branch TYPE j_1bnfdoc-branch,
         parid  TYPE j_1bnfdoc-parid,
         series TYPE j_1bnfdoc-series,
         nfnum  TYPE j_1bnfdoc-nfnum,
         nfenum TYPE j_1bnfdoc-nfenum,
         cancel TYPE j_1bnfdoc-cancel,
         nftype TYPE j_1bnfdoc-nftype,
       END   OF type_doc,

       BEGIN OF type_lfa1,
         lifnr TYPE lfa1-lifnr,
         land1 TYPE lfa1-land1,
         name1 TYPE lfa1-name1,
         regio TYPE lfa1-regio,
       END   OF type_lfa1,

       BEGIN OF type_retlote,
         docnum     TYPE zsdt_retlote-docnum,
         nfenum     TYPE zsdt_retlote-nfenum,
         werks      TYPE zsdt_retlote-werks,
         nf_retorno TYPE zsdt_retlote-nf_retorno,
         docnum_ret TYPE zsdt_retlote-docnum_ret,
         quant_vinc TYPE zsdt_retlote-quant_vinc,
       END   OF type_retlote,

       BEGIN OF type_saida,
         bukrs       TYPE j_1bnfdoc-bukrs,
         branch      TYPE j_1bnfdoc-branch,
         branch_mb51 TYPE j_1bnfdoc-branch,
         depos_mb51  TYPE j_1bnfdoc-branch,
         charg       TYPE j_1bnflin-charg,
         matnr       TYPE j_1bnflin-matnr,
         saldo       TYPE ccm_quant,
         saldo_mb51  TYPE ccm_quant,
         dif         TYPE ccm_quant,
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

       BEGIN OF type_mb51,
         anln1      TYPE  mseg-anln1,
         anln2      TYPE  mseg-anln2,
         aplzl      TYPE  mseg-aplzl,
         aufnr      TYPE  mseg-aufnr,
         aufpl      TYPE  mseg-aufpl,
*         bktxt      TYPE  mkpf-bktxt,
*         bldat      TYPE  mkpf-bldat,
         bpmng      TYPE  mseg-bpmng,
         bprme      TYPE  mseg-bprme,
         bstme      TYPE  mseg-bstme,
         bstmg      TYPE  mseg-bstmg,
*         budat      TYPE  mkpf-budat,
         bukrs      TYPE  mseg-bukrs,
         bwart      TYPE  mseg-bwart,
         bwtar      TYPE  mseg-bwtar,
         charg      TYPE  mseg-charg,
*         cpudt      TYPE  mkpf-cpudt,
*         cputm      TYPE  mkpf-cputm,
         dmbtr      TYPE  mseg-dmbtr,
         ebeln      TYPE  mseg-ebeln,
         ebelp      TYPE  mseg-ebelp,
         erfme      TYPE  mseg-erfme,
         erfmg      TYPE  mseg-erfmg,
         exbwr      TYPE  mseg-exbwr,
         exvkw      TYPE  mseg-exvkw,
         grund      TYPE  mseg-grund,
         kdauf      TYPE  mseg-kdauf,
         kdein      TYPE  mseg-kdein,
         kdpos      TYPE  mseg-kdpos,
         kostl      TYPE  mseg-kostl,
         kunnr      TYPE  mseg-kunnr,
         kzbew      TYPE  mseg-kzbew,
         kzvbr      TYPE  mseg-kzvbr,
         kzzug      TYPE  mseg-kzzug,
         lgort      TYPE  mseg-lgort,
         lifnr      TYPE  mseg-lifnr,
         matnr      TYPE  mseg-matnr,
         mat_kdauf  TYPE  mseg-mat_kdauf,
         mat_kdpos  TYPE  mseg-mat_kdpos,
*         mblnr      TYPE  mkpf-mblnr,
         meins      TYPE  mseg-meins,
         menge      TYPE  mseg-menge,
*         mjahr      TYPE  mkpf-mjahr,
         nplnr      TYPE  mseg-nplnr,
         ps_psp_pnr TYPE  mseg-ps_psp_pnr,
         rsnum      TYPE  mseg-rsnum,
         rspos      TYPE  mseg-rspos,
         shkzg      TYPE  mseg-shkzg,
         sobkz      TYPE  mseg-sobkz,
*         usnam      TYPE  mkpf-usnam,
*         vgart      TYPE  mkpf-vgart,
         vkwrt      TYPE  mseg-vkwrt,
         waers      TYPE  mseg-waers,
         werks      TYPE  mseg-werks,
*         xabln      TYPE  mkpf-xabln,
         xauto      TYPE  mseg-xauto,
*         xblnr      TYPE  mkpf-xblnr,
         zeile      TYPE  mseg-zeile,
         maa_urzei  TYPE  mseg-maa_urzei,
         xmacc      TYPE  mseg-xmacc,
       END OF type_mb51,

       BEGIN OF ty_j_1bnfnad,
         parid  TYPE j_1bnfnad-parid,
         name1  TYPE j_1bnfnad-name1,
         docnum TYPE j_1bnfnad-docnum,
         parvw  TYPE j_1bnfnad-parvw,
       END OF ty_j_1bnfnad.

TYPES: BEGIN OF ty_zcarta.
         INCLUDE TYPE zcarta_correcao.
TYPES: END OF ty_zcarta.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_zsdt0002    TYPE TABLE OF type_zsdt0002,
      it_zfiwrt0008 TYPE TABLE OF ty_zfiwrt0008,
      wa_zfiwrt0008 TYPE          ty_zfiwrt0008,
      it_zfiwrt0009 TYPE TABLE OF ty_zfiwrt0009,
      it_zfiwrt0015 TYPE TABLE OF zfiwrt0015 WITH HEADER LINE,
      t_lin         TYPE TABLE OF type_lin,
      t_lin2        TYPE TABLE OF type_lin,
      t_dd07t       TYPE TABLE OF dd07t,
      t_doc         TYPE TABLE OF type_doc,
      t_mb51        TYPE TABLE OF type_mb51,
      w_mb51        TYPE type_mb51,
      t_depara      TYPE TABLE OF zsdt_depara_depo,
      w_depara      TYPE zsdt_depara_depo,
      t_active      TYPE TABLE OF j_1bnfe_active,
      t_doc2        TYPE TABLE OF type_doc,
      t_likp        TYPE TABLE OF type_likp,
      t_lips        TYPE TABLE OF type_lips,
      t_vbrp        TYPE TABLE OF type_vbrp,
      t_lfa1        TYPE TABLE OF type_lfa1,
      t_retlote     TYPE TABLE OF type_retlote,
      t_sumret      TYPE TABLE OF type_retlote,
      t_zsdt0033    TYPE TABLE OF zsdt0033,
      t_export      TYPE TABLE OF zsdt_export,
      t_vbfa        TYPE TABLE OF type_vbfa,
      t_saida       TYPE TABLE OF type_saida,
      t_saida_aux   TYPE TABLE OF type_saida,
      t_fcat        TYPE TABLE OF lvc_s_fcat,
      t_j_1bnfnad   TYPE TABLE OF ty_j_1bnfnad,
      it_parce_car  TYPE TABLE OF ty_j_1bnfnad,
      wa_parce_car  TYPE          ty_j_1bnfnad,
      it_zcarta     TYPE TABLE OF ty_zcarta,
      wa_zcarta     TYPE          ty_zcarta,
      t_tool        TYPE          ui_functions,
      s_cont        TYPE REF TO   cl_gui_custom_container,
      s_alv         TYPE REF TO   cl_gui_alv_grid,
      s_layout      TYPE lvc_s_layo.

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*
DATA: t_bapi_material_tab TYPE TABLE OF bapi2017_gm_material_ra,
      t_bapi_plant_tab    TYPE TABLE OF bapi2017_gm_plant_ra,
      t_bapi_date_tab     TYPE TABLE OF bapi2017_gm_pstng_date_ra,
      t_batch_ra          TYPE TABLE OF bapi2017_gm_batch_ra,
      t_goodsmvt_header   TYPE TABLE OF bapi2017_gm_head_02,
      t_goodsmvt_items    TYPE TABLE OF bapi2017_gm_item_show,
      t_return_tab        TYPE TABLE OF bapiret2,
      w_bapi_material_tab TYPE bapi2017_gm_material_ra,
      w_bapi_plant_tab    TYPE bapi2017_gm_plant_ra,
      w_bapi_date_tab     TYPE bapi2017_gm_pstng_date_ra,
      w_batch_ra          TYPE bapi2017_gm_batch_ra,
      w_goodsmvt_header   TYPE bapi2017_gm_head_02,
      w_goodsmvt_items    TYPE bapi2017_gm_item_show,
      w_return_tab        TYPE bapiret2.

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
    s_werks  FOR t001w-werks OBLIGATORY                           ,
    s_wvirt  FOR zsdt_depara_depo-werks_v                         ,
    s_matnr  FOR lips-matnr OBLIGATORY                            ,
    s_charg  FOR j_1bnflin-charg   NO INTERVALS NO-EXTENSION      ,
    s_erdat  FOR likp-erdat  OBLIGATORY NO-EXTENSION                         .

SELECTION-SCREEN END   OF BLOCK a1.

PARAMETERS p_report(10) TYPE c NO-DISPLAY.

AT SELECTION-SCREEN OUTPUT.
*
*  IF s_charg[] IS NOT INITIAL.
*    IF s_erdat[] IS INITIAL.
*      IF s_charg-low IS INITIAL.
*        READ TABLE s_charg INTO s_charg INDEX 1.
*      ENDIF.
*      s_erdat-sign = 'I'.
*      s_erdat-option = 'BT'.
*      s_erdat-low = s_charg-low && '0101'.
*      s_erdat-high = s_charg-low && '1231'.
*      APPEND s_erdat.
*    ELSE.
*      READ TABLE s_erdat INTO s_erdat INDEX 1.
*      READ TABLE s_charg INTO s_charg INDEX 1.
*      s_erdat-low(4) = s_charg-low.
*      s_erdat-high(4) = s_charg-low.
*      MODIFY s_erdat INDEX 1.
*    ENDIF.
*  ELSEIF s_erdat[] IS NOT INITIAL.
*    READ TABLE s_erdat INTO s_erdat INDEX 1.
*    IF sy-subrc = 0.
*      s_charg-sign = 'I'.
*      s_charg-option = 'EQ'.
*      s_charg-low = s_erdat-low(4).
*      APPEND s_charg.
*      s_erdat-high(4) = s_charg-low.
*      MODIFY s_erdat INDEX 1.
*    ENDIF.
*  ENDIF.


*----------------------------------------------------------------------*
*                           Start of Selection                         *
*----------------------------------------------------------------------*
START-OF-SELECTION.

*  IF s_charg[] IS NOT INITIAL.
*    IF s_erdat[] IS INITIAL.
*      READ TABLE s_charg INTO s_charg INDEX 1.
*      s_erdat-sign = 'I'.
*      s_erdat-option = 'BT'.
*      s_erdat-low = s_charg-low && '0101'.
*      s_erdat-high = s_charg-low && '1231'.
*      APPEND s_erdat.
*    ELSE.
*      READ TABLE s_erdat INTO s_erdat INDEX 1.
*      READ TABLE s_charg INTO s_charg INDEX 1.
*      s_erdat-low(4) = s_charg-low.
*      s_erdat-high(4) = s_charg-low.
*      MODIFY s_erdat INDEX 1.
*    ENDIF.
*  ELSEIF s_erdat[] IS NOT INITIAL.
*    READ TABLE s_erdat INTO s_erdat INDEX 1.
*    IF sy-subrc = 0.
*      s_charg-sign = 'I'.
*      s_charg-option = 'EQ'.
*      s_charg-low = s_erdat-low(4).
*      APPEND s_charg.
*      s_erdat-high(4) = s_charg-low.
*      MODIFY s_erdat INDEX 1.
*    ENDIF.
*  ENDIF.

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
*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255
  SELECT * INTO TABLE t_depara
    FROM zsdt_depara_depo
     WHERE werks IN s_werks
       AND werks_v IN s_wvirt.
*  zcl_depara_centro_fixo_virtual=>get_dados_depara(
*      EXPORTING
*        i_werks       = CONV #( s_werks )
*      IMPORTING
*       e_table_depara         = t_depara  ).

*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255  - FIM



  SORT t_depara BY  werks.

  IF t_depara[] IS NOT INITIAL.
    LOOP AT t_depara INTO w_depara WHERE werks_v IN s_wvirt. "Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255  -
      s_wvirt-sign = 'I'.
      s_wvirt-option = 'EQ'.
      s_wvirt-low = w_depara-werks_v.
      APPEND s_wvirt.
    ENDLOOP.
    SORT s_wvirt BY low.
    DELETE ADJACENT DUPLICATES FROM s_wvirt COMPARING low.
  ENDIF.

* PERFORM zf_exec_mb51.
  PERFORM zf_exec_bapi_mb51.
* PERFORM zf_exec_mb51_background.

  SORT t_mb51 BY matnr werks.

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

           z_seleciona_active.

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

ENDFORM.                    " Z_SELECIONA_ZSDT0002

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIKP                                         *
*&---------------------------------------------------------------------*
*                            Seleciona LIKP                            *
*----------------------------------------------------------------------*
FORM z_seleciona_likp.

  RANGES: r_fkarv FOR likp-fkarv.

  REFRESH: t_likp, r_fkarv.

  CHECK NOT t_zsdt0002[] IS INITIAL.

  LOOP AT t_zsdt0002 INTO DATA(w_zsdt0002).
    r_fkarv-sign   = 'I'.
    r_fkarv-option = 'EQ'.
    r_fkarv-low    = w_zsdt0002-fkart.
    APPEND r_fkarv.
  ENDLOOP.

  SELECT vbeln erdat vstel
         vkorg kunnr fkarv
    FROM likp
    INTO TABLE t_likp
  WHERE  erdat IN s_erdat
    AND  vstel IN s_werks
    AND  vkorg IN s_bukrs
    AND  fkarv IN r_fkarv.

  DELETE t_likp WHERE vstel NOT IN s_werks.
  DELETE t_likp WHERE vkorg NOT IN s_bukrs.
  DELETE t_likp WHERE fkarv NOT IN r_fkarv.

*  SELECT vbeln erdat vstel
*         vkorg kunnr fkarv
*    FROM likp
*    INTO TABLE t_likp
*    FOR ALL ENTRIES IN t_zsdt0002
*  WHERE  erdat IN s_erdat
*    AND  vstel IN s_werks
*    AND  vkorg IN s_bukrs
*    AND  fkarv EQ t_zsdt0002-fkart.

  SORT t_likp BY vbeln ASCENDING.
  CHECK t_likp[] IS INITIAL.
  CHECK it_zfiwrt0008[] IS INITIAL.

ENDFORM.                    " Z_SELECIONA_LIKP

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIPS                                         *
*&---------------------------------------------------------------------*
*                           Seleciona LIPS                             *
*----------------------------------------------------------------------*
FORM z_seleciona_lips.

  REFRESH t_lips.

  CHECK NOT t_likp[] IS INITIAL.

  SELECT vbeln posnr matnr lgort
    FROM lips
    INTO TABLE t_lips
    FOR ALL ENTRIES IN t_likp
  WHERE  vbeln EQ t_likp-vbeln
    AND  matnr IN s_matnr.

  SORT t_lips BY vbeln ASCENDING
                 posnr ASCENDING.
  CHECK t_lips[] IS INITIAL.
  CHECK it_zfiwrt0008[] IS INITIAL.

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

  CHECK NOT t_lin[] IS INITIAL.
  tl_lin[] = t_lin[].
  SORT tl_lin BY docnum ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_lin COMPARING docnum.

  SELECT docnum docdat bukrs
         branch parid  series
         nfnum  nfenum cancel nftype
    FROM j_1bnfdoc
    INTO TABLE t_doc
     FOR ALL ENTRIES IN tl_lin
   WHERE docnum EQ tl_lin-docnum.

  DELETE t_doc WHERE cancel = 'X' AND  nftype <> 'ZJ'.

  SELECT parid name1 docnum parvw
    FROM j_1bnfnad
    INTO TABLE t_j_1bnfnad
    FOR ALL ENTRIES IN t_doc
  WHERE docnum = t_doc-docnum.
  SORT t_j_1bnfnad BY parvw.

  DELETE t_j_1bnfnad WHERE  parvw NE 'Z1'.

  LOOP AT t_doc INTO wa_doc.

    READ TABLE t_j_1bnfnad INTO wa_nad WITH KEY docnum = wa_doc-docnum
                                                parvw  = 'Z1'.

    IF ( sy-subrc EQ 0 ).
      CONTINUE.
    ELSE.
      DELETE t_doc WHERE docnum EQ wa_doc-docnum.
    ENDIF.

  ENDLOOP.


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
         nf_retorno docnum_ret quant_vinc
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

  DATA: sl_doc       TYPE type_doc,
        sl_doc2      TYPE type_doc,
        sl_active    TYPE j_1bnfe_active,
        sl_lin       TYPE type_lin,
        sl_lfa1      TYPE type_lfa1,
        sl_sumret    TYPE type_retlote,
        sl_retlote   TYPE type_retlote,
        sl_export    TYPE zsdt_export,
        sl_vbfa      TYPE type_vbfa,
        sl_lin2      TYPE type_lin,
        sl_saida     TYPE type_saida,
        sl_saida_aux TYPE type_saida,
        sl_dd07t     TYPE dd07t,
        sl_j_1bnfnad TYPE ty_j_1bnfnad,
        index        TYPE sy-index,
        retlot       TYPE c LENGTH 1,
        index_doc    TYPE sy-index,
        wa_lfa1      TYPE lfa1.

  DATA: wl_zlest0146 TYPE zlest0146,
        lt_zlest0147 TYPE zlest0147_t,
        lt_zlest0168 TYPE zlest0168_t,
        v_doc_rateio TYPE char01.

  DATA: lv_tabix TYPE sy-tabix.

  SORT: it_zfiwrt0008 BY docnum,
        it_zfiwrt0015 BY seq_lcto.

  SORT t_depara BY werks werks_v lgort.
  DELETE ADJACENT DUPLICATES FROM t_depara
                        COMPARING werks werks_v lgort.

  REFRESH t_saida.

  LOOP AT t_doc INTO sl_doc.

    READ TABLE t_lin INTO sl_lin WITH KEY docnum = sl_doc-docnum.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
    READ TABLE t_sumret INTO sl_sumret
      WITH KEY docnum = sl_doc-docnum
      BINARY SEARCH.

    sl_saida-bukrs   = sl_doc-bukrs.
    sl_saida-branch  = sl_doc-branch.
    sl_saida-matnr   = sl_lin-matnr.
    sl_saida-charg   = sl_lin-charg.
    sl_saida-saldo_mb51 = 0.
    READ TABLE t_depara INTO DATA(w_depara) WITH KEY werks = sl_doc-branch. " BINARY SEARCH.
    IF sy-subrc = 0.
      sl_saida-branch_mb51 = w_depara-werks_v.
      sl_saida-depos_mb51 = w_depara-lgort.
    ENDIF.
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
        sl_saida-saldo = sl_lin-menge - sl_sumret-quant_vinc.  "Original
      ELSE.
        sl_saida-saldo = 0.
      ENDIF.

      COLLECT sl_saida INTO t_saida.

    ENDLOOP.

    READ TABLE t_retlote TRANSPORTING NO FIELDS WITH KEY docnum = sl_doc-docnum.
    IF sy-subrc IS NOT INITIAL.
      sl_saida-saldo = sl_lin-menge.
    ENDIF.

**** RETLOTE WSB FIM

    IF retlot IS INITIAL.
      COLLECT sl_saida INTO t_saida.
    ENDIF.

    CLEAR: sl_lin, "Modificado por Welgem incluido fora do loop do retlote
     sl_saida.

    CLEAR: sl_doc,
           sl_lfa1,
           sl_sumret,
           sl_retlote,
           sl_export,
           sl_vbfa,
           sl_lin2,
           retlot,
           sl_dd07t.

  ENDLOOP.

  SORT t_depara BY werks.

  REFRESH t_saida_aux[].

*------------------------------------
* SAIDA RESUMO
*------------------------------------
  LOOP AT t_saida INTO sl_saida.
    lv_tabix = sy-tabix.
    sl_saida-saldo_mb51 = 0.

    LOOP AT t_depara INTO w_depara WHERE werks = sl_saida-branch.

      READ TABLE t_mb51 INTO w_mb51 WITH KEY matnr = sl_saida-matnr
                                             werks = w_depara-werks_v
                                             lgort = w_depara-lgort
                                             charg = sl_saida-charg.
      IF sy-subrc = 0.
        sl_saida_aux = sl_saida.
        sl_saida_aux-saldo_mb51 = w_mb51-erfmg.
        sl_saida_aux-branch_mb51 = w_depara-werks_v.
        sl_saida_aux-depos_mb51 = w_depara-lgort.
        sl_saida_aux-dif = 0.

        READ TABLE t_saida_aux INTO DATA(w_saida_aux)
                               WITH KEY matnr  = sl_saida-matnr
                                        branch = sl_saida-branch
                                        charg  = sl_saida-charg.
        IF sy-subrc = 0.
*       IF t_saida_aux[] IS NOT INITIAL.
          sl_saida_aux-saldo = 0.
          sl_saida-dif = sl_saida-dif + w_mb51-erfmg.
        ELSE.
          sl_saida-dif = w_mb51-erfmg - sl_saida-saldo.
        ENDIF.
        APPEND sl_saida_aux TO t_saida_aux .
      ENDIF.
    ENDLOOP.

    READ TABLE t_saida_aux INTO w_saida_aux
                           WITH KEY matnr  = sl_saida-matnr
                                    branch = sl_saida-branch
                                    charg  = sl_saida-charg.
    IF sy-subrc = 0.
      MODIFY t_saida_aux FROM sl_saida INDEX sy-tabix TRANSPORTING  dif.
    ENDIF.
  ENDLOOP.

  t_saida[] = t_saida_aux[].

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
    c_table 'BUKRS'         TEXT-006 10 space space,
    c_table 'BRANCH'        TEXT-007 10 space space,
    c_table 'BRANCH_MB51'   TEXT-008 15 space space,
    c_table 'DEPOS_MB51'    TEXT-009 15 space space,
    c_table 'CHARG'         TEXT-029 10 space space,
    c_table 'MATNR'         TEXT-014 20 c_x   space,
    c_table 'SALDO_MB51'    TEXT-022 20 space space,
    c_table 'SALDO'         TEXT-021 20 space space,
    c_table 'DIF'           TEXT-023 20 space space.

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
* s_layout-cwidth_opt = 'X'.

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

ENDFORM.                    " Z_INST_ALV


*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ALV                                              *
*&---------------------------------------------------------------------*
*                                Exibe Alv                             *
*----------------------------------------------------------------------*
FORM z_exibe_alv.

  DATA vl_int TYPE int4.

  CALL METHOD s_alv->set_table_for_first_display
    EXPORTING
      i_default                     = c_x
      is_layout                     = s_layout
*     it_toolbar_excluding          = t_tool
    CHANGING
      it_outtab                     = t_saida
      it_fieldcatalog               = t_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

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
*&      Form  Z_SELECIONA_ZFIWRT0008
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_zfiwrt0008 .

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

  ENDIF.

ENDFORM.                    " Z_CARTA_CORRECAO

FORM z_seleciona_active.

  CHECK t_doc[] IS NOT INITIAL.

  SELECT *
    FROM j_1bnfe_active INTO TABLE t_active
    FOR ALL ENTRIES IN t_doc
  WHERE docnum = t_doc-docnum.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  zf_exec_MB51.
*&---------------------------------------------------------------------*
FORM zf_exec_bapi_mb51.

  FREE: t_bapi_material_tab,
        t_bapi_plant_tab,
        t_bapi_date_tab,
        t_batch_ra.

  LOOP AT s_matnr.
    w_bapi_material_tab-sign     = s_matnr-sign.
    w_bapi_material_tab-option   = s_matnr-option.
    w_bapi_material_tab-low      = s_matnr-low.
    w_bapi_material_tab-high     = s_matnr-high.
    APPEND w_bapi_material_tab  TO t_bapi_material_tab.
  ENDLOOP.

  LOOP AT s_wvirt.
    w_bapi_plant_tab-sign        = s_wvirt-sign.
    w_bapi_plant_tab-option      = s_wvirt-option.
    w_bapi_plant_tab-low         = s_wvirt-low.
    w_bapi_plant_tab-high        = s_wvirt-high.
    APPEND w_bapi_plant_tab     TO t_bapi_plant_tab.
  ENDLOOP.

  LOOP AT s_erdat.
    w_bapi_date_tab-sign         = s_erdat-sign.
    w_bapi_date_tab-option       = s_erdat-option.
    w_bapi_date_tab-low          = s_erdat-low.
    w_bapi_date_tab-high         = s_erdat-high.
    APPEND w_bapi_date_tab      TO t_bapi_date_tab.
  ENDLOOP.

  LOOP AT s_charg.
    w_batch_ra-sign              = s_charg-sign.
    w_batch_ra-option            = s_charg-option.
    w_batch_ra-low               = s_charg-low.
    w_batch_ra-high              = s_charg-high.
    APPEND w_batch_ra           TO t_batch_ra.
  ENDLOOP.

*-----------------------------------
*-efetua MB51
*-----------------------------------
  CALL FUNCTION 'ZBAPI_GOODSMVT_GETITEMS'
    EXPORTING
      i_selec_direto  = abap_true
    TABLES
      material_ra     = t_bapi_material_tab
      plant_ra        = t_bapi_plant_tab
*     STGE_LOC_RA     =
      batch_ra        = t_batch_ra
*     MOVE_TYPE_RA    =
*     SPEC_STOCK_RA   =
*     TR_EV_TYPE_RA   =
      pstng_date_ra   = t_bapi_date_tab
*     VENDOR_RA       =
*     USERNAME_RA     =
      goodsmvt_header = t_goodsmvt_header
      goodsmvt_items  = t_goodsmvt_items
      return          = t_return_tab.

*-----------------------------------
*-resultado
*-----------------------------------
  LOOP AT t_goodsmvt_items INTO w_goodsmvt_items.
    CLEAR w_mb51.
    w_mb51-bukrs      = s_bukrs-low.
    w_mb51-lgort      = w_goodsmvt_items-stge_loc.
    w_mb51-matnr      = w_goodsmvt_items-material.
    w_mb51-werks      = w_goodsmvt_items-plant.
    w_mb51-charg      = w_goodsmvt_items-batch.
    w_mb51-erfmg      = w_goodsmvt_items-entry_qnt.
    COLLECT w_mb51 INTO t_mb51.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  zf_exec_MB51.
*&---------------------------------------------------------------------*
FORM zf_exec_mb51.

  DATA: BEGIN OF tl_listout OCCURS 0,
          line(1024) TYPE c,
        END OF tl_listout.

  DATA: tl_seltab    TYPE TABLE OF rsparams,
        el_seltab_wa LIKE LINE OF tl_seltab.

  DATA: vl_col0  TYPE c.
  DATA: vl_htype LIKE dd01v-datatype.

  DATA: vl_dtfim  TYPE sy-datum.

  DATA: vl_variant TYPE disvariant-variant.

  DATA: tl_bdcdata TYPE TABLE OF bdcdata,
        el_bdcdata LIKE LINE OF tl_bdcdata.

  FIELD-SYMBOLS: <fs_tab>  TYPE ANY TABLE,
                 <fs_line> TYPE any.
  FIELD-SYMBOLS: <fs_value> TYPE any.

  DATA: lf_ref  TYPE REF TO data,
        lf_ref1 TYPE REF TO data.

  DATA el_opt TYPE ctu_params.


  CLEAR el_bdcdata.
  el_bdcdata-program = 'RM07DOCS'.
  el_bdcdata-dynpro = '1000'.
  el_bdcdata-dynbegin = 'X'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'BDC_CURSOR'.
  el_bdcdata-fval = 'MATNR-LOW'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'BDC_OKCODE'.
  el_bdcdata-fval = '=%001'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

*  el_bdcdata-fnam = 'BDC_OKCODE'.
*  el_bdcdata-fval = '=NONE'.
*  APPEND el_bdcdata TO tl_bdcdata.
*  CLEAR el_bdcdata.

  LOOP AT s_matnr.
    CLEAR el_bdcdata.
    el_bdcdata-program = 'SAPLALDB'.
    el_bdcdata-dynpro = '3000'.
    el_bdcdata-dynbegin = 'X'.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
    el_bdcdata-fnam = 'BDC_CURSOR'.
    el_bdcdata-fval = 'RSCSEL_255-SLOW_I(01)'.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
    el_bdcdata-fnam = 'RSCSEL_255-SLOW_I(01)'.
    el_bdcdata-fval = s_matnr-low..
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
    el_bdcdata-fnam = 'BDC_OKCODE'.
    el_bdcdata-fval = '=LINS'.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
  ENDLOOP.

  el_bdcdata-fnam = 'BDC_OKCODE'.
  el_bdcdata-fval = '=ACPT'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  CLEAR el_bdcdata.
  el_bdcdata-program = 'RM07DOCS'.
  el_bdcdata-dynpro = '1000'.
  el_bdcdata-dynbegin = 'X'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'BDC_CURSOR'.
  el_bdcdata-fval = 'WERKS-LOW'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'BDC_OKCODE'.
  el_bdcdata-fval = '=%002'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

*  el_bdcdata-fnam = 'BDC_OKCODE'.
*  el_bdcdata-fval = '=NONE'.
*  APPEND el_bdcdata TO tl_bdcdata.
*  CLEAR el_bdcdata.

  LOOP AT s_wvirt.
    CLEAR el_bdcdata.
    el_bdcdata-program = 'SAPLALDB'.
    el_bdcdata-dynpro = '3000'.
    el_bdcdata-dynbegin = 'X'.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
    el_bdcdata-fnam = 'BDC_CURSOR'.
    el_bdcdata-fval = 'RSCSEL_255-SLOW_I(01)'.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
    el_bdcdata-fnam = 'RSCSEL_255-SLOW_I(01)'.
    el_bdcdata-fval = s_wvirt-low.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
    el_bdcdata-fnam = 'BDC_OKCODE'.
    el_bdcdata-fval = '=LINS'.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
  ENDLOOP.

  el_bdcdata-fnam = 'BDC_OKCODE'.
  el_bdcdata-fval = '=ACPT'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  CLEAR el_bdcdata.
  el_bdcdata-program = 'RM07DOCS'.
  el_bdcdata-dynpro = '1000'.
  el_bdcdata-dynbegin = 'X'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  LOOP AT s_charg.
    el_bdcdata-fnam = 'CHARG-LOW'.
    el_bdcdata-fval = s_charg-low.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
  ENDLOOP.

  el_bdcdata-fnam = 'BUDAT-LOW'.
  el_bdcdata-fval = s_erdat-low+6(2) && '.' && s_erdat-low+4(2) && '.' && s_erdat-low(4).
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'BUDAT-HIGH'.
  el_bdcdata-fval = s_erdat-high+6(2) && '.' && s_erdat-high+4(2) && '.' && s_erdat-high(4).
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'RFLAT_L'.
  el_bdcdata-fval = 'X'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'ALV_DEF'.
  el_bdcdata-fval = '//'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'DATABASE'.
  el_bdcdata-fval = 'X'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'BDC_OKCODE'.
  el_bdcdata-fval = '=ONLI'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_opt-dismode = 'N'.

  cl_salv_bs_runtime_info=>set(
  EXPORTING display = abap_false
  metadata = abap_true
  data = abap_true ).


  CALL TRANSACTION 'MB51' USING tl_bdcdata OPTIONS FROM el_opt.

  TRY.

      cl_salv_bs_runtime_info=>get_data_ref(
      IMPORTING r_data = lf_ref ).

      ASSIGN lf_ref->* TO <fs_tab>.

    CATCH cx_salv_bs_sc_runtime_info.


  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  IF <fs_tab> IS ASSIGNED.

    CREATE DATA lf_ref1 LIKE LINE OF <fs_tab>.

    ASSIGN lf_ref1->* TO <fs_line>.

    REFRESH t_mb51.

    LOOP AT <fs_tab> ASSIGNING <fs_line>.

      ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        w_mb51-bukrs = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'LGORT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        w_mb51-lgort = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        w_mb51-matnr = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'WERKS' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        w_mb51-werks = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'ERFMG' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        w_mb51-erfmg = <fs_value>.
      ENDIF.

      COLLECT w_mb51 INTO t_mb51.
    ENDLOOP.
  ENDIF.
ENDFORM.                    "zf_exec_FBL3N

*&---------------------------------------------------------------------*
*&      Form  zf_exec_mb51_background.
*&---------------------------------------------------------------------*
FORM zf_exec_mb51_background.
  FIELD-SYMBOLS: <fs_tab>  TYPE ANY TABLE,
                 <fs_line> TYPE any.
  FIELD-SYMBOLS: <fs_value> TYPE any.

  DATA: lf_ref  TYPE REF TO data,
        lf_ref1 TYPE REF TO data.
  DATA: t_listout LIKE abaplist OCCURS 0 WITH HEADER LINE.

  DATA: tl_seltab    TYPE TABLE OF rsparams,
        el_seltab_wa LIKE LINE OF tl_seltab.

  DATA: vl_variant TYPE disvariant-variant,
        vl_monitor TYPE rihea-pm_selfield.

  DATA: vl_erdat TYPE sy-datum.
  DATA: vl_pster TYPE sy-datum.

  DATA: vl_col0 TYPE c.
  DATA: vl_col1 TYPE c.

  DATA: vl_time     TYPE  p VALUE 6,
        vl_timeunit TYPE  char1 VALUE 'D'.

  REFRESH tl_seltab.
  REFRESH t_listout.

  LOOP AT s_matnr.
    el_seltab_wa-selname = 'MATNR'.
    el_seltab_wa-kind    = 'S'.
    el_seltab_wa-sign    = 'I'.
    el_seltab_wa-option  = 'EQ'.
    el_seltab_wa-low = s_matnr-low.
    el_seltab_wa-high = s_matnr-high.
    APPEND el_seltab_wa TO tl_seltab.
  ENDLOOP.

  LOOP AT s_charg.
    el_seltab_wa-selname = 'CHARG'.
    el_seltab_wa-kind    = 'S'.
    el_seltab_wa-sign    = 'I'.
    el_seltab_wa-option  = 'EQ'.
    el_seltab_wa-low = s_charg-low.
    el_seltab_wa-high = s_charg-high.
    APPEND el_seltab_wa TO tl_seltab.
  ENDLOOP.

  LOOP AT s_wvirt.
    el_seltab_wa-selname = 'WERKS'.
    el_seltab_wa-sign    = 'I'.
    el_seltab_wa-kind    = 'S'.
    el_seltab_wa-option  = 'EQ'.
    el_seltab_wa-low = s_wvirt-low.
    el_seltab_wa-high = s_wvirt-high.
    APPEND el_seltab_wa TO tl_seltab.
  ENDLOOP.

  vl_erdat = sy-datum.
  el_seltab_wa-selname = 'BUDAT'.
  el_seltab_wa-sign    = 'I'.
  el_seltab_wa-option  = 'BT'.
  el_seltab_wa-kind    = 'S'.
  el_seltab_wa-low = s_erdat-low.
  el_seltab_wa-high = s_erdat-high.
  APPEND el_seltab_wa TO tl_seltab.

  el_seltab_wa-selname = 'ALV_DEF'.
  el_seltab_wa-sign    = 'I'.
  el_seltab_wa-option  = 'EQ'.
  el_seltab_wa-low = '//'.
  el_seltab_wa-high = ''.

  APPEND el_seltab_wa TO tl_seltab.
  cl_salv_bs_runtime_info=>set(
  EXPORTING display = abap_false
  metadata = abap_true
  data = abap_true ).

  SUBMIT rm07docs WITH SELECTION-TABLE tl_seltab
              EXPORTING LIST TO MEMORY
              AND RETURN.

*  PERFORM zf_retrieve_list_from_memory TABLES t_listout.
  TRY.

      cl_salv_bs_runtime_info=>get_data_ref(
      IMPORTING r_data = lf_ref ).

      ASSIGN lf_ref->* TO <fs_tab>.

    CATCH cx_salv_bs_sc_runtime_info.


  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  IF <fs_tab> IS ASSIGNED.

    CREATE DATA lf_ref1 LIKE LINE OF <fs_tab>.

    ASSIGN lf_ref1->* TO <fs_line>.

    REFRESH t_mb51.

    LOOP AT <fs_tab> ASSIGNING <fs_line>.

      ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        w_mb51-bukrs = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'LGORT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        w_mb51-lgort = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        w_mb51-matnr = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'WERKS' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        w_mb51-werks = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'ERFMG' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        w_mb51-erfmg = <fs_value>.
      ENDIF.

      COLLECT w_mb51 INTO t_mb51.
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f_retrieve_list_from_memory
*&---------------------------------------------------------------------*
FORM zf_retrieve_list_from_memory TABLES tl_reportlines.

  DATA: tl_list LIKE abaplist OCCURS 0 WITH HEADER LINE.
  DATA: tl_txtlines(1024) TYPE c OCCURS 0 WITH HEADER LINE.

  CLEAR tl_list.  REFRESH tl_list.
  CLEAR tl_reportlines. REFRESH tl_reportlines.

  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      listobject = tl_list
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'LIST_TO_ASCI'
    TABLES
      listobject         = tl_list
      listasci           = tl_txtlines
    EXCEPTIONS
      empty_list         = 1
      list_index_invalid = 2
      OTHERS             = 3.

  CHECK sy-subrc = 0.

  tl_reportlines[] = tl_txtlines[].

  CALL FUNCTION 'LIST_FREE_MEMORY'.

ENDFORM.                    "retrieve_list_from_memory
