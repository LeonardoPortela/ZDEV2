*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDI0001                                                *
* Descrição  : Controle para retorno de formação de lote               *
* Módulo     : SD                                Transação: ZSDT0008   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 26/07/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zsdi0001 NO STANDARD PAGE HEADING MESSAGE-ID sd.

TABLES: t001l,
        kna1 ,
        mara ,
        mch1 ,
        lfa1 ,
        sscrfields,
        zdco_produtor,
        j_1bnflin,
        zsdt0170, "DU-e 26.06.2018 - Ini
        likp,
        znom_transporte,
        zsdt0053,
        lips.
*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_vbrk,
         vbeln  TYPE vbrk-vbeln,
         fkart  TYPE vbrk-fkart,
         kunrg  TYPE vbrk-kunrg,
         xblnr  TYPE vbrk-xblnr,
         refkey TYPE j_1bnflin-refkey,
       END   OF type_vbrk,

       BEGIN OF type_vbrp,
         vbeln TYPE vbrp-vbeln,
         posnr TYPE vbrp-posnr,
         fkimg TYPE vbrp-fkimg,
         matnr TYPE vbrp-matnr,
         werks TYPE vbrp-werks,
         lgort TYPE vbrp-lgort,
         charg TYPE vbrp-charg,
         netwr TYPE vbrp-netwr,
       END   OF type_vbrp,

       BEGIN OF type_vbrp_aux,
         refkey TYPE j_1bnflin-refkey,
         posnr  TYPE vbrp-posnr,
         vgbel  TYPE vbrp-vgbel,
       END OF type_vbrp_aux,

       BEGIN OF type_doc,
         docnum TYPE j_1bnfdoc-docnum,
         credat TYPE j_1bnfdoc-credat,
         nfenum TYPE j_1bnfdoc-nfenum,
         bukrs  TYPE j_1bnfdoc-bukrs,
         branch TYPE j_1bnfdoc-branch,
         kunnr  TYPE j_1bnfdoc-parid,
         partyp TYPE j_1bnfdoc-partyp,
         parvw  TYPE j_1bnfdoc-parvw,
       END   OF type_doc,

       BEGIN OF type_correc,
         docnum        TYPE zcarta_correcao-docnum,
         novo_terminal TYPE zcarta_correcao-novo_terminal,
         id_cc         TYPE zcarta_correcao-id_cc,
       END OF type_correc,

       BEGIN OF type_lin,
         docnum     TYPE j_1bnflin-docnum,
         itmnum     TYPE j_1bnflin-itmnum,
         matnr      TYPE j_1bnflin-matnr,
         maktx      TYPE j_1bnflin-maktx,
         bwkey      TYPE j_1bnflin-bwkey,
         matkl      TYPE j_1bnflin-matkl,
         nbm        TYPE j_1bnflin-nbm,
         taxsit     TYPE j_1bnflin-taxsit,
         taxsi2     TYPE j_1bnflin-taxsi2,
         matuse     TYPE j_1bnflin-matuse,
         refkey     TYPE j_1bnflin-refkey,
         menge      TYPE j_1bnflin-menge,
         meins      TYPE j_1bnflin-meins,
         netpr      TYPE j_1bnflin-netpr,
         netwr      TYPE j_1bnflin-netwr,
         netwrt     TYPE j_1bnflin-netwrt,
         taxlw1     TYPE j_1bnflin-taxlw1,
         taxlw2     TYPE j_1bnflin-taxlw2,
         itmtyp     TYPE j_1bnflin-itmtyp,
         werks      TYPE j_1bnflin-werks,
         taxlw3     TYPE j_1bnflin-taxlw3,
         taxlw4     TYPE j_1bnflin-taxlw4,
         seq_lcto	  TYPE zfiwed006,
         reftyp     TYPE j_1bnflin-reftyp,
         ctrl_zrfl  TYPE zfiwrt0008-ctrl_zrfl,
         del        TYPE c,
         dtachegada TYPE zlest0019-dtachegada,
       END   OF type_lin,

       BEGIN OF type_act,
         docnum TYPE j_1bnfe_active-docnum,
         docsta TYPE j_1bnfe_active-docsta,
         cancel TYPE j_1bnfe_active-cancel,
       END   OF type_act,

       BEGIN OF type_vbpa,
         vbeln  TYPE vbpa-vbeln,
         posnr  TYPE vbpa-posnr,
         parvw  TYPE vbpa-parvw,
         kunnr  TYPE vbpa-kunnr,
         lifnr  TYPE vbpa-lifnr,
         refkey TYPE j_1bnflin-refkey,
       END   OF type_vbpa,

       BEGIN OF type_desc,
         butxt TYPE t001-butxt,
         vtext TYPE zsdt0002-vtext,
         name1 TYPE t001w-name1,
         lgobe TYPE t001l-lgobe,
         cli   TYPE kna1-name1,
         parc  TYPE lfa1-name1,
         maktx TYPE makt-maktx,
       END   OF type_desc,

       BEGIN OF type_avinc,
         docnum     TYPE j_1bnfdoc-docnum,
         credat     TYPE j_1bnfdoc-credat,
         dtachegada TYPE zlest0019-dtachegada,
         nfenum     TYPE j_1bnfdoc-nfenum,
         menge      TYPE j_1bnflin-menge,
         netwrt     TYPE j_1bnflin-netwrt,
         saldo      TYPE j_1bnflin-menge,
         status     TYPE zstatus_lote,
         marc       TYPE char1,
       END   OF type_avinc,

       BEGIN OF type_aux,
         menge  TYPE j_1bnflin-menge,
         netwrt TYPE j_1bnflin-netwrt,
         nftot  TYPE j_1bnftot,
       END   OF type_aux,

       BEGIN OF type_nf_terc,
         "LIFNR   TYPE LIFNR,
         check TYPE c,
       END OF type_nf_terc,

       BEGIN OF type_erro,
         tipo   TYPE bapi_mtype,
         numero TYPE bapi_mtype,
         msg    TYPE bapi_msg,
       END   OF type_erro,

       BEGIN OF type_vbfa,
         vbelv   TYPE vbfa-vbelv,
         posnv   TYPE vbfa-posnv,
         vbeln   TYPE vbfa-vbeln,
         posnn   TYPE vbfa-posnn,
         vbtyp_n TYPE vbfa-vbtyp_n,
         vbtyp_v TYPE vbfa-vbtyp_v,
         erdat   TYPE vbfa-erdat,
         erzet   TYPE vbfa-erzet,
       END   OF type_vbfa,

***    Ch.129231 - Marcos Faneli
       BEGIN OF ty_vin_dco,
         vbeln  TYPE vbfa-vbeln,
         nr_dco TYPE zdco_vinculo-nr_dco,
       END OF ty_vin_dco,

       BEGIN OF ty_dco,
         docnum TYPE j_1bnflin-docnum,
       END OF ty_dco,

       BEGIN OF ty_likp,
         vbeln TYPE vbeln_vl,
       END OF ty_likp,

       BEGIN OF ty_lips,
         vbeln TYPE vbeln_vl,
         posnr TYPE posnr,
         vgbel TYPE vgbel,
         vgpos TYPE vgpos,
       END OF ty_lips,

       BEGIN OF ty_zsdt_retlote,
         docnum     LIKE zsdt_retlote-docnum,
         werks      LIKE zsdt_retlote-werks,
         nf_retorno LIKE zsdt_retlote-nf_retorno,
         docnum_ret LIKE zsdt_retlote-docnum_ret,
         nfenum     LIKE zsdt_retlote-nfenum,
         quant_vinc LIKE zsdt_retlote-quant_vinc,
       END OF ty_zsdt_retlote.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_saida_0101,
         chave_nfe TYPE c LENGTH 44,
         menge     TYPE j_1bnflin-menge.
TYPES: END OF ty_saida_0101.

DATA: obj_alv_0101       TYPE REF TO cl_gui_alv_grid,
      obj_container_0101 TYPE REF TO cl_gui_custom_container.


DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.

* ALV field catalogs
DATA: it_fcat TYPE lvc_t_fcat,
      wa_fcat TYPE lvc_s_fcat.

* ALV excluded functions
DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

* Alv Styles
DATA: ls_edit TYPE lvc_s_styl,
      lt_edit TYPE lvc_t_styl.

* ALV layout variant
DATA: gs_variant       TYPE disvariant.

* ALV layout
DATA: gs_layout        TYPE lvc_s_layo.

* ALV Stable
DATA: wa_stable        TYPE lvc_s_stbl.

DATA: it_selectedcell TYPE lvc_t_cell,
      wa_selectedcell TYPE lvc_s_cell.

DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: gt_estilo TYPE lvc_t_styl WITH HEADER LINE,
      wl_estilo TYPE lvc_s_styl.

DATA: gt_f4  TYPE lvc_t_f4 WITH HEADER LINE.

DATA: r_eudr TYPE RANGE OF zeudr.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

DATA: wa_estrutura TYPE ty_estrutura,
      estrutura    TYPE TABLE OF ty_estrutura.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_vbrk             TYPE TABLE OF type_vbrk,
      t_vbrp             TYPE TABLE OF type_vbrp,
      t_vbpa             TYPE TABLE OF type_vbpa,
      t_doc              TYPE TABLE OF type_doc,
      t_correc           TYPE TABLE OF type_correc,
      t_lin              TYPE TABLE OF type_lin      WITH HEADER LINE,
      t_act              TYPE TABLE OF type_act,
      t_ret              TYPE TABLE OF ty_zsdt_retlote,
      t_avinc            TYPE TABLE OF type_avinc,
      t_avinc_bkp        TYPE TABLE OF type_avinc WITH HEADER LINE,
      t_vinc             TYPE TABLE OF type_avinc,
      t_avinc_aux        TYPE TABLE OF type_avinc,
      t_erro             TYPE TABLE OF type_erro,
      t_vbfa             TYPE TABLE OF type_vbfa,
      t_vbfa_3           TYPE TABLE OF type_vbfa,
      t_vbfa_2           TYPE TABLE OF type_vbfa,
      t_vbfa_n           TYPE TABLE OF type_vbfa     WITH HEADER LINE,
      t_likp             TYPE TABLE OF ty_likp,
*      T_LIPS        TYPE TABLE OF TY_LIPS,
      t_lips             TYPE TABLE OF ty_lips,
***    Ch.129231 - Marcos Faneli
      t_vin_dco          TYPE TABLE OF ty_vin_dco,
      t_dco              TYPE TABLE OF ty_dco,
      cor(4)             VALUE 2,
      itensidade(1)      VALUE 1,
      t_zib_nfe_dist_ter TYPE TABLE OF zib_nfe_dist_ter,
      t_zib_nfe_dist_itm TYPE TABLE OF zib_nfe_dist_itm,
      t_zsdt_export      TYPE TABLE OF zsdt_export.

DATA: w_zib_nfe_dist_ter TYPE  zib_nfe_dist_ter.
DATA: _wl_0170 TYPE zsdt0170. "// WBARBOSA 31102024 US-153330

DATA: it_saida_0101 TYPE TABLE OF ty_saida_0101,
      it_ret_ter    TYPE TABLE OF zsdt_retlote_ter.

DATA: it_rfl_zsdt0008  TYPE zsdt0001_ro_vinc_t WITH HEADER LINE. "DU-e 26.06.2018

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*
DATA: v_index      TYPE syindex,
      tg_tvarvc    TYPE TABLE OF tvarvc,
      it_zlest0039 TYPE TABLE OF zlest0039,
      ws_zlest0039 TYPE zlest0039,
      rg_final     TYPE RANGE OF zsdt_export-finalidade,
      v_erro       TYPE i.

CONTROLS: tc_avinc TYPE TABLEVIEW USING SCREEN '0100',
          tc_vinc  TYPE TABLEVIEW USING SCREEN '0100'.

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
CONSTANTS: c_parvw TYPE vbpa-parvw VALUE 'Z1'.

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
DATA: s_desc    TYPE type_desc,
      s_avinc   TYPE type_avinc,
      s_vinc    TYPE type_avinc,
      s_val     TYPE type_aux,
      s_nf_terc TYPE type_nf_terc,
      s_dco     TYPE znr_dco,
      s_pqsdco  TYPE c LENGTH 1.


*----------------------------------------------------------------------*
*** Campo da tela 0200
*----------------------------------------------------------------------*
DATA: BEGIN OF wg_0200,
        docnum_ret TYPE zsdt_retlote-docnum_ret,
        id_redex   TYPE  zsdt_retlote-id_export,
      END OF wg_0200.

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.
    PARAMETERS:
      p_bukrs TYPE t001-bukrs, "     OBLIGATORY,
      p_fkart TYPE zsdt0002-fkart MATCHCODE OBJECT zsdt0002, " OBLIGATORY,
      p_werks TYPE t001w-werks. "    OBLIGATORY.
    SELECT-OPTIONS:
      s_lgort FOR t001l-lgort NO-EXTENSION NO INTERVALS DEFAULT 'ARMZ'," OBLIGATORY,
      s_safra FOR mch1-charg  NO-EXTENSION NO INTERVALS," OBLIGATORY,
      s_kunnr FOR kna1-kunnr  NO-EXTENSION NO INTERVALS," OBLIGATORY,
*  S_PARC  FOR LFA1-LIFNR  NO-EXTENSION NO INTERVALS," OBLIGATORY,
      s_parc  FOR lfa1-lifnr  NO INTERVALS," OBLIGATORY,
      s_matnr FOR mara-matnr  NO-EXTENSION NO INTERVALS," OBLIGATORY.
      s_inst FOR zsdt0053-instrucao NO-EXTENSION NO INTERVALS, "ZSDT0008 - Adicionar filtro instrucao - BG #128346
      s_nbm   FOR j_1bnflin-nbm  NO-EXTENSION NO INTERVALS,
      s_docnum FOR j_1bnflin-docnum NO-EXTENSION NO INTERVALS. "// wbarbosa 31102024 US-153330
    "S_NOM   FOR ZNOM_TRANSPORTE-ID_NOMEACAO_TRAN NO-EXTENSION NO INTERVALS MATCHCODE OBJECT Z_PSQ_NOM_TRANSPORTE.
    PARAMETERS:
      p_final TYPE zsdt_export-finalidade, "ZFIN_EXPORT, " OBLIGATORY.
      p_quant TYPE zsdt0053-zmeng. " OBLIGATORY.
***    Ch.129231 - Marcos Faneli
    SELECT-OPTIONS:
      s_dco_   FOR zdco_produtor-nr_dco," OBLIGATORY.
      s_ov     FOR lips-vgbel NO INTERVALS. "DEVK9A229G - SD - Incluir filtro OV. transação ZSDT0008 #143752 RSA

    PARAMETERS: p_depfec TYPE c AS CHECKBOX.
    PARAMETERS: p_redex TYPE c AS CHECKBOX  USER-COMMAND r1. "US - 81360
    PARAMETERS: p_eudr TYPE c AS CHECKBOX   USER-COMMAND r2. "// WBARBOSA 31102024 US-153330

    PARAMETERS: p_cct_cp TYPE c AS CHECKBOX.

    PARAMETERS: p_tcode TYPE syst-tcode NO-DISPLAY.
    PARAMETERS: p_idprd TYPE lfa1-lifnr NO-DISPLAY.


    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 03(65) TEXT-ibr.
    SELECTION-SCREEN END OF LINE.


    "DU-e 26.06.2018 - Ini

    "CCT
    SELECTION-SCREEN BEGIN OF BLOCK a4 WITH FRAME TITLE TEXT-036.
      SELECTION-SCREEN BEGIN OF LINE.

        PARAMETERS: p_comcct RADIOBUTTON GROUP rb2 MODIF ID a4 USER-COMMAND u_cct.
        SELECTION-SCREEN COMMENT 03(12) TEXT-033 FOR FIELD p_comcct.

        PARAMETERS: p_semcct RADIOBUTTON GROUP rb2 MODIF ID a4.
        SELECTION-SCREEN COMMENT 17(12) TEXT-034 FOR FIELD p_semcct.

        PARAMETERS: p_allcct RADIOBUTTON GROUP rb2 DEFAULT 'X' MODIF ID a4.
        SELECTION-SCREEN COMMENT 35(12) TEXT-035 FOR FIELD p_allcct.

      SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN END OF BLOCK a4.

    "DU-e
    SELECTION-SCREEN BEGIN OF BLOCK a5 WITH FRAME TITLE TEXT-037.

      SELECT-OPTIONS: p_id_due FOR zsdt0170-id_due             NO INTERVALS NO-EXTENSION MODIF ID a5,
                      p_nrdue  FOR zsdt0170-numero_due         NO INTERVALS NO-EXTENSION MODIF ID a5,
                      p_raemb  FOR zsdt0170-codigo_ra_embarque NO INTERVALS NO-EXTENSION MODIF ID a5.

    SELECTION-SCREEN END OF BLOCK a5.

    "DU-e 26.06.2018 - Fim


  SELECTION-SCREEN END OF BLOCK a2.

  SELECTION-SCREEN BEGIN OF BLOCK a3 WITH FRAME TITLE TEXT-027.
    PARAMETERS: p_nfenum TYPE j_1bnfdoc-nfenum            MODIF ID a3,
                p_series TYPE j_1bnfdoc-series            MODIF ID a3,
                p_docdat TYPE j_1bnfdoc-docdat            MODIF ID a3,
                p_netwr  TYPE wrbtr                       MODIF ID a3,
                p_chvnfe TYPE zib_nfe_dist_ter-chave_nfe  MODIF ID a3.

  SELECTION-SCREEN END OF BLOCK a3.

SELECTION-SCREEN END OF BLOCK a1.


SELECT-OPTIONS: p_vbeln FOR likp-vbeln NO-DISPLAY.

*----------------------------------------------------------------------*
*                         AT SELECTION SCREEN                          *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  CLEAR: s_nf_terc, v_erro.

  IF sy-ucomm = 'FC01'.
    CALL SCREEN 0200 STARTING AT 10 10.
  ENDIF.

  "// WBARBOSA 31102024 US-153330
  IF sy-ucomm EQ 'R2'.
    FREE r_eudr.
  ENDIF.
  "// WBARBOSA 31102024 US-153330

  CHECK sy-ucomm NE '%008'.

  PERFORM z_valida_emp.
  IF v_erro IS INITIAL.
    PERFORM z_valida_tid.
  ENDIF.
  IF v_erro IS INITIAL.
    PERFORM z_valida_cen.
  ENDIF.
  IF v_erro IS INITIAL.
    PERFORM z_valida_dep.
  ENDIF.
  IF v_erro IS INITIAL.
    PERFORM z_valida_lote.
  ENDIF.
  IF v_erro IS INITIAL.
    PERFORM z_valida_cli.
  ENDIF.
  IF v_erro IS INITIAL.
    PERFORM z_valida_ent.
  ENDIF.
  IF v_erro IS INITIAL.
    PERFORM z_valida_mat.
  ENDIF.
  IF v_erro IS INITIAL.
    IF p_final IS INITIAL.
      ADD 1 TO v_erro.
      MESSAGE s836 WITH 'Finalidade é obrigatório.'.
    ENDIF.
  ENDIF.
  IF v_erro IS INITIAL.
    IF p_final IS INITIAL.
      ADD 1 TO v_erro.
      MESSAGE s836 WITH 'Quantidade é obrigatório.'.
    ENDIF.
  ENDIF.

  IF ( v_erro    IS INITIAL     ) AND
     ( ( s_nf_terc IS NOT INITIAL ) OR ( p_redex = 'X') ) . "US - 81360 - REDEX - CBRAND
    PERFORM z_valida_nf_terc.
  ENDIF.

  IF ( p_depfec IS NOT INITIAL ) AND ( s_nf_terc IS INITIAL ).
    MESSAGE s836 WITH 'Remessa Depósito Fechado selecionado,' 'e não informado NF 3º!'.
    ADD 1 TO v_erro.
  ENDIF.

  IF v_erro IS NOT INITIAL.
    LEAVE TO SCREEN 1000.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

*"// WBARBOSA 31102024 US-153330
    IF screen-name EQ 'P_EUDR'.
      IF _wl_0170-eudr EQ zcl_eudr_utils=>lc_n_eudr.
        screen-active = '1'.
      ELSE.
        screen-active = '0'.
      ENDIF.
    ENDIF.
*"// WBARBOSA 31102024 US-153330

    IF screen-name EQ 'P_CCT_CP'.
      IF p_semcct EQ abap_true.
        screen-active = '1'.
      ELSE.
        screen-active = '0'.
        CLEAR: p_cct_cp.
      ENDIF.
    ENDIF.

    CASE screen-group1.
      WHEN 'A3'. "Controle Campos Nota Terceiro
        IF ( s_nf_terc-check IS NOT INITIAL ) AND p_redex IS INITIAL. "US - 81360 - REDEX - CBRAND
          screen-active = '1'.
          IF screen-name EQ 'P_CHVNFE' OR screen-name EQ '%_P_CHVNFE_%_APP_%-TEXT'.
            screen-active = '0'.
            CLEAR: p_chvnfe.
          ENDIF.
        ELSE.
          screen-active = '0'.
        ENDIF.
*** US - 81360 - REDEX - CBRAND - inicio
        IF p_redex = 'X'.
          IF screen-name EQ 'P_CHVNFE' OR screen-name EQ '%_P_CHVNFE_%_APP_%-TEXT'.
            screen-active = '1'.
          ELSE.
            IF  screen-name EQ  'P_NFENUM' OR
                screen-name EQ  'P_SERIES' OR
                screen-name EQ  'P_DOCDAT' OR
                screen-name EQ  'P_NETWR'.
              screen-active = '0'.
            ENDIF.
          ENDIF.
        ENDIF.
*** US - 81360 - REDEX - CBRAND - fim

        "WHEN 'A4' OR 'A5'. "Registro CCT/DU-e
      WHEN 'A5'. "Registro CCT/DU-e
        screen-input = '0'.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF v_erro IS INITIAL.

    PERFORM f_check_lcto_terc USING v_erro.
    CHECK v_erro IS INITIAL.

* Seleção Dados
    PERFORM z_seleciona_dados.

* Monta Tabela a Vincular
    PERFORM z_monta_avincular.
    CALL SCREEN '0100'.
  ENDIF.


*----------------------------------------------------------------------*
*** Adiciona botões ao menu superior
*----------------------------------------------------------------------*
INITIALIZATION.

  "DU-e 26.06.2018 - Ini
  DATA: v_comcct      TYPE c,
        v_semcct      TYPE c,
        v_allcct      TYPE c,
        v_id_due      TYPE zsdt0170-id_due,
        v_tcode       TYPE syst-tcode,
        v_id_produtor TYPE znom_remetente-id_remetente.
  "DU-e 26.06.2018 - Fim

  SELECTION-SCREEN : FUNCTION KEY 1.
  sscrfields-functxt_01 = 'Desvincular Retorno'.

  "DU-e 26.06.2018 - Ini
  CLEAR: it_rfl_zsdt0008[].
  IMPORT it_rfl_zsdt0008 FROM MEMORY ID 'IT_RFL_ZSDT0008'.

  CLEAR: v_comcct, v_semcct, v_allcct, v_id_due, v_tcode, v_id_produtor.

  IMPORT v_comcct       FROM MEMORY ID 'P_COMCCT'.
  IMPORT v_semcct       FROM MEMORY ID 'P_SEMCCT'.
  IMPORT v_allcct       FROM MEMORY ID 'P_ALLCCT'.
  IMPORT v_id_due       FROM MEMORY ID 'P_ID_DUE'.
  IMPORT v_tcode        FROM MEMORY ID 'P_TCODE'.
  IMPORT v_id_produtor  FROM MEMORY ID 'P_ID_PRODUTOR'.

  IF ( v_comcct IS NOT INITIAL ) OR
     ( v_semcct IS NOT INITIAL ) OR
     ( v_allcct IS NOT INITIAL ).

    DELETE FROM MEMORY ID 'P_COMCCT'.
    DELETE FROM MEMORY ID 'P_SEMCCT'.
    DELETE FROM MEMORY ID 'P_ALLCCT'.

    p_comcct = v_comcct.
    p_semcct = v_semcct.
    p_allcct = v_allcct.
  ENDIF.

  IF v_tcode IS NOT INITIAL.
    p_tcode = v_tcode.
    DELETE FROM MEMORY ID 'P_TCODE'.
  ENDIF.

  IF v_id_produtor IS NOT INITIAL.
    p_idprd = v_id_produtor.
    DELETE FROM MEMORY ID 'P_ID_PRODUTOR'.
  ENDIF.

  IF v_id_due IS NOT INITIAL.

    SELECT SINGLE *
      FROM zsdt0170 INTO _wl_0170
     WHERE id_due EQ v_id_due.

    IF sy-subrc EQ 0.
      p_id_due-sign   = 'I'.
      p_id_due-option = 'EQ'.
      p_id_due-low    = _wl_0170-id_due.
      APPEND p_id_due.

      p_nrdue-sign   = 'I'.
      p_nrdue-option = 'EQ'.
      p_nrdue-low    = _wl_0170-numero_due.
      APPEND p_nrdue.

      p_raemb-sign   = 'I'.
      p_raemb-option = 'EQ'.
      p_raemb-low    = _wl_0170-codigo_ra_embarque.
      APPEND p_raemb.

*"// WBARBOSA 31102024 - US-153330
      FREE: r_eudr.
      CASE _wl_0170-eudr.
        WHEN zcl_eudr_utils=>lc_s_eudr.

          APPEND VALUE #( sign   = 'I'
                          option = 'EQ'
                          low    = zcl_eudr_utils=>lc_s_eudr ) TO r_eudr.

        WHEN zcl_eudr_utils=>lc_n_eudr.

          APPEND VALUE #( sign   = 'I'
                          option = 'NE'
                          low    = zcl_eudr_utils=>lc_s_eudr ) TO r_eudr.

          IF p_eudr IS NOT INITIAL. "Lista tambem notas EUDR?
            FREE r_eudr.
          ENDIF.
      ENDCASE.
*"// WBARBOSA 31102024 - US-153330
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0168 INTO @DATA(_wl_0168)
     WHERE codigo_ra EQ @_wl_0170-codigo_ra_embarque.

    IF ( sy-subrc EQ 0 ) AND
       ( _wl_0170-codigo_ra_embarque IS NOT INITIAL ) AND
       ( _wl_0168-lifnr IS NOT INITIAL ).
      s_parc-sign   = 'I'.
      s_parc-option = 'EQ'.
      s_parc-low    = _wl_0168-lifnr.
      APPEND s_parc.
    ENDIF.

  ENDIF.
  "DU-e 26.06.2018 - Fim

*&---------------------------------------------------------------------*
*&      Form  Z_VALIDA_EMP                                             *
*&---------------------------------------------------------------------*
*                             Válida Empresa                           *
*----------------------------------------------------------------------*
FORM z_valida_emp.

  DATA wa_t001k TYPE t001k.
  CLEAR s_desc-butxt.

  IF p_bukrs IS INITIAL.
    MESSAGE s836 WITH 'Informe empresa.'.
    ADD 1 TO v_erro.
  ENDIF.

  CHECK NOT p_bukrs IS INITIAL.

  SELECT SINGLE bukrs butxt
    FROM t001
    INTO (p_bukrs, s_desc-butxt)
  WHERE  bukrs EQ p_bukrs.

  IF sy-subrc NE 0.
    MESSAGE e836 WITH TEXT-002.
  ELSE.

    CHECK NOT p_werks IS INITIAL.

    SELECT SINGLE werks name1
      FROM t001w
      INTO (p_werks, s_desc-name1)
    WHERE  werks EQ p_werks.

    IF sy-subrc NE 0.
      MESSAGE e836 WITH TEXT-004.
    ELSE.
      SELECT SINGLE *
        FROM t001k
        INTO wa_t001k
        WHERE bwkey = p_werks.
      IF sy-subrc EQ 0.
        IF wa_t001k-bukrs NE p_bukrs.
          MESSAGE e836 WITH 'Centro' p_werks 'não existe na empresa' p_bukrs.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_VALIDA_EMP

*&---------------------------------------------------------------------*
*&      Form  Z_VALIDA_TID                                             *
*&---------------------------------------------------------------------*
*                        Válida Tipo Documento                         *
*----------------------------------------------------------------------*
FORM z_valida_tid.

  CLEAR s_desc-vtext.

  IF p_fkart IS INITIAL.
    MESSAGE s836 WITH 'Informe tipo documento.'.
    ADD 1 TO v_erro.
  ENDIF.

  CHECK NOT p_fkart IS INITIAL.

  SELECT SINGLE fkart vtext
    FROM zsdt0002
    INTO (p_fkart, s_desc-vtext)
  WHERE  fkart EQ p_fkart.

  CHECK NOT sy-subrc IS INITIAL.

  MESSAGE e836 WITH TEXT-003.

ENDFORM.                    " Z_VALIDA_TID

*&---------------------------------------------------------------------*
*&      Form  Z_VALIDA_CEN                                             *
*&---------------------------------------------------------------------*
*                              Válida Centro                           *
*----------------------------------------------------------------------*
FORM z_valida_cen.

  DATA: wa_t001k           TYPE t001k,
        wa_zsdt_depara_cen TYPE zsdt_depara_cen.

  CLEAR s_desc-name1.

  IF p_werks IS INITIAL.
    MESSAGE s836 WITH 'Informe o centro.'.
    ADD 1 TO v_erro.
  ENDIF.

  CHECK NOT p_werks IS INITIAL.

  SELECT SINGLE werks name1
    FROM t001w
    INTO (p_werks, s_desc-name1)
  WHERE  werks EQ p_werks.

  IF sy-subrc NE 0.
    MESSAGE e836 WITH TEXT-004.
  ELSE.
    SELECT SINGLE *
      FROM t001k
      INTO wa_t001k
      WHERE bwkey = p_werks.
    IF sy-subrc EQ 0.
      IF wa_t001k-bukrs NE p_bukrs.
        MESSAGE e836 WITH 'Centro' p_werks 'não existe na empresa' p_bukrs.
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
    FROM zsdt_depara_cen
    INTO wa_zsdt_depara_cen
    WHERE vkorg       = p_bukrs
    AND   centro_real = p_werks.

  IF sy-subrc NE 0.
    MESSAGE e836 WITH 'Centro' p_werks 'não é centro real' p_bukrs.
  ENDIF.

ENDFORM.                    " Z_VALIDA_CEN

*&---------------------------------------------------------------------*
*&      Form  Z_VALIDA_DEP                                             *
*&---------------------------------------------------------------------*
*                           Válida Depósito                            *
*----------------------------------------------------------------------*
FORM z_valida_dep.

  CLEAR s_desc-lgobe.

  IF s_lgort IS INITIAL.
    MESSAGE s836 WITH 'Informe o depósito.'.
    ADD 1 TO v_erro.
  ENDIF.

  CHECK NOT s_lgort[] IS INITIAL.

  SELECT SINGLE lgort lgobe
    FROM t001l
    INTO (s_lgort-low, s_desc-lgobe)
  WHERE  werks EQ p_werks
    AND  lgort EQ s_lgort-low.

  CHECK NOT sy-subrc IS INITIAL.

  MESSAGE s836 WITH TEXT-005 p_werks.

ENDFORM.                    " Z_VALIDA_DEP

*&---------------------------------------------------------------------*
*&      Form  Z_VALIDA_CLI                                             *
*&---------------------------------------------------------------------*
*                             Válida Cliente                           *
*----------------------------------------------------------------------*
FORM z_valida_cli.

  CLEAR: s_desc-cli.

  IF s_kunnr IS INITIAL.
    MESSAGE s836 WITH 'Informe o cliente.'.
    ADD 1 TO v_erro.
  ENDIF.

  CHECK NOT s_kunnr[] IS INITIAL.

  SELECT SINGLE kunnr name1
    FROM kna1
    INTO (s_kunnr-low, s_desc-cli)
  WHERE  kunnr EQ s_kunnr-low.

  IF sy-subrc NE 0.
    MESSAGE e836 WITH TEXT-006.
    EXIT.
  ELSE.
    PERFORM f_check_lcto_terc CHANGING v_erro.
  ENDIF.

ENDFORM.                    " Z_VALIDA_CLI

*&---------------------------------------------------------------------*
*&      Form  Z_VALIDA_ENT                                             *
*&---------------------------------------------------------------------*
*                        Válida Cliente de Entrega                     *
*----------------------------------------------------------------------*
FORM z_valida_ent.

  CLEAR s_desc-parc.

  IF s_parc IS INITIAL.
    MESSAGE s836 WITH 'Informe terminal de entrega.'.
    ADD 1 TO v_erro.
  ENDIF.

  CHECK NOT s_parc[] IS INITIAL.

  LOOP AT s_parc.

    SELECT SINGLE lifnr name1
      FROM lfa1
      INTO (s_parc-low, s_desc-parc)
    WHERE  lifnr EQ s_parc-low.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s836 WITH TEXT-007.
      ADD 1 TO v_erro.
    ENDIF.

  ENDLOOP.

  CHECK v_erro IS INITIAL.

  IF lines( s_parc[] ) NE 1.

    SELECT *
      FROM lfa1
      INTO TABLE @DATA(it_cnpj)
    WHERE lifnr IN @s_parc.

    SORT it_cnpj BY stcd1.
    DELETE ADJACENT DUPLICATES FROM it_cnpj COMPARING stcd1.

    IF lines( it_cnpj ) NE 1.
      MESSAGE s836 WITH TEXT-038.
      ADD 1 TO v_erro.
    ENDIF.

    SELECT *
      FROM lfa1
      INTO TABLE @DATA(it_cnpj_ativo)
    WHERE lifnr IN @s_parc
      AND ( loevm = ' ' OR sperr = ' ' OR sperm = ' ' ).

    IF sy-subrc IS INITIAL.
      READ TABLE it_cnpj_ativo INTO DATA(wa_cnpj_ativo) INDEX 1.
      s_parc-low = wa_cnpj_ativo-lifnr.
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_VALIDA_ENT

*&---------------------------------------------------------------------*
*&      Form  Z_VALIDA_MAT                                             *
*&---------------------------------------------------------------------*
*                           Válida Material                            *
*----------------------------------------------------------------------*
FORM z_valida_mat.

  CLEAR s_desc-maktx.

  IF s_matnr IS INITIAL.
    MESSAGE s836 WITH 'Informe Material.'.
    ADD 1 TO v_erro.
  ENDIF.

  CHECK NOT s_matnr[] IS INITIAL.

  SELECT SINGLE matnr
    FROM mara
    INTO s_matnr-low
  WHERE  matnr EQ s_matnr-low.

  SELECT SINGLE maktx
    FROM makt
    INTO s_desc-maktx
  WHERE  matnr EQ s_matnr-low
    AND  spras EQ sy-langu.

  CHECK NOT sy-subrc IS INITIAL.

  MESSAGE e836 WITH TEXT-008.

ENDFORM.                    " Z_VALIDA_MAT

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                            Seleção Dados                             *
*----------------------------------------------------------------------*
FORM z_seleciona_dados.

  CLEAR: it_saida_0101[], it_ret_ter[].

*  IF S_NOM IS INITIAL.

  PERFORM: "Seleciona J_1BNFLIN ZW
            z_seleciona_lin_zw,
            "Seleciona  Z_SELECIONA_LIN_REM_DEP_FEC
            z_seleciona_lin_rem_dep_fec,
            "Seleciona Cabeçalho das Remessas
            z_seleciona_likp  .

  IF ( t_likp[] IS NOT INITIAL ).
    PERFORM:
       "Seleciona Itens da Remessa
       z_seleciona_lips.
    IF ( t_lips[] IS NOT INITIAL ).
      PERFORM:
          "Seleciona Parceiro
          z_seleciona_vbpa_2,
          "Selecionar VBFA
          z_seleciona_vbfa  ,
          "Seleciona VBRK
          z_seleciona_vbrk_2,
          "Seleciona J_1BNFLIN
          z_seleciona_lin   .
    ENDIF.
  ENDIF.
*  ELSE.
*    PERFORM: Z_SELECIONA_LOGISTICA.
*  ENDIF.

  PERFORM: z_processa_lin    ,
           z_seleciona_doc   ,"   Seleciona J_1BNFDOC
           z_seleciona_act   ."   Seleciona J_1BNFE_ACTIVE

***  Ch. 129321 - Marcos Faneli
*  Seleciona notas vicnuladas DCO
  IF s_dco_ IS NOT INITIAL.
    PERFORM z_seleciona_dco.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBRK                                         *
*&---------------------------------------------------------------------*
*                             Seleciona VBRK                           *
*----------------------------------------------------------------------*
FORM z_seleciona_vbrk.

  DATA: sl_vbrk  TYPE type_vbrk.

  REFRESH t_vbrk.

  SELECT vbeln fkart kunrg xblnr
    FROM vbrk
    INTO TABLE t_vbrk
  WHERE  fkart EQ p_fkart
    AND  bukrs EQ p_bukrs AND draft = space .

  SORT t_vbrk BY vbeln ASCENDING.

  LOOP AT t_vbrk INTO sl_vbrk.

    v_index = sy-tabix.

    sl_vbrk-refkey = sl_vbrk-vbeln.
    MODIFY t_vbrk FROM sl_vbrk
      INDEX v_index
      TRANSPORTING refkey.

    CLEAR sl_vbrk.

  ENDLOOP.

  IF t_vbrk[] IS INITIAL.
    MESSAGE i836 WITH TEXT-009.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_VBRK

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBRP                                         *
*&---------------------------------------------------------------------*
*                              Seleciona VBRP                          *
*----------------------------------------------------------------------*
FORM z_seleciona_vbrp.

  DATA: sl_aubel TYPE vbeln_va.

  REFRESH t_vbrp.

  CHECK NOT t_vbrk[] IS INITIAL.

  IF s_dco IS NOT INITIAL.

    SELECT SINGLE vbeln INTO sl_aubel
      FROM zdco_produtor
     WHERE nr_dco EQ s_dco.

    SELECT vbeln posnr fkimg
           matnr werks lgort charg netwr
      FROM vbrp
      INTO TABLE t_vbrp
       FOR ALL ENTRIES IN t_vbrk
     WHERE vbeln EQ t_vbrk-vbeln
       AND aubel EQ sl_aubel.

    IF t_vbrp[] IS INITIAL.
      MESSAGE i836 WITH s_dco TEXT-021.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ELSE.

    SELECT vbeln posnr fkimg
           matnr werks lgort charg netwr
      FROM vbrp AS vb
      INTO TABLE t_vbrp
       FOR ALL ENTRIES IN t_vbrk
     WHERE vbeln EQ t_vbrk-vbeln
       AND NOT EXISTS ( SELECT * FROM zdco_produtor AS zp WHERE zp~vbeln EQ vb~aubel ).

  ENDIF.

  SORT t_vbrp BY vbeln ASCENDING
                 posnr ASCENDING.

  DELETE t_vbrp WHERE werks NE p_werks.

  IF NOT s_safra[] IS INITIAL.
    DELETE t_vbrp WHERE charg NOT IN s_safra.
  ENDIF.

  IF NOT s_lgort[] IS INITIAL.
    DELETE t_vbrp WHERE lgort NOT IN s_lgort.
  ENDIF.

  IF NOT s_matnr[] IS INITIAL.
    DELETE t_vbrp WHERE matnr NOT IN s_matnr.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_VBRP

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DOC                                          *
*&---------------------------------------------------------------------*
*                           Seleciona J_1BNFDOC                        *
*----------------------------------------------------------------------*
FORM z_seleciona_doc.

  DATA: tl_lin TYPE TABLE OF type_lin,
        sl_doc TYPE type_doc.

  REFRESH t_doc.

  tl_lin[] = t_lin[].
  SORT tl_lin BY docnum ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_lin COMPARING docnum.


  SELECT docnum credat nfenum bukrs
         branch parid partyp parvw
    FROM j_1bnfdoc
    INTO TABLE t_doc
    FOR ALL ENTRIES IN tl_lin
  WHERE docnum EQ tl_lin-docnum.

  SORT t_doc BY docnum ASCENDING.

  IF NOT s_kunnr[] IS INITIAL.
    DELETE t_doc WHERE kunnr NOT IN s_kunnr.
  ENDIF.
  IF t_doc[] IS INITIAL.
    MESSAGE i836 WITH TEXT-012.
    LEAVE LIST-PROCESSING.
  ENDIF.

  READ TABLE t_doc INTO sl_doc INDEX 1.
  s_kunnr-low = sl_doc-kunnr.
  SELECT SINGLE name1
    FROM kna1
    INTO s_desc-cli
  WHERE  kunnr EQ s_kunnr-low.

ENDFORM.                    " Z_SELECIONA_DOC

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ACT                                          *
*&---------------------------------------------------------------------*
*                       Seleciona J_1BNFE_ACTIVE                       *
*----------------------------------------------------------------------*
FORM z_seleciona_act.

  REFRESH t_act.

  SELECT docnum docsta cancel
    FROM j_1bnfe_active
    INTO TABLE t_act
    FOR ALL ENTRIES IN t_doc
  WHERE  docnum EQ t_doc-docnum.

  SORT t_act BY docnum ASCENDING.

  DELETE t_act WHERE docsta NE '1'
                  OR cancel NE space.

  IF t_act[] IS INITIAL.
    MESSAGE i836 WITH TEXT-012.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_ACT

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBPA                                         *
*&---------------------------------------------------------------------*
*                           Seleciona VBPA                             *
*----------------------------------------------------------------------*
FORM z_seleciona_vbpa.

  SELECT vbeln posnr parvw kunnr
    FROM vbpa
    INTO CORRESPONDING FIELDS OF TABLE t_vbpa
    FOR ALL ENTRIES IN t_vbrp
  WHERE  vbeln EQ t_vbrp-vbeln
    AND  posnr EQ t_vbrp-posnr
    AND  parvw EQ c_parvw.

  SORT t_vbpa BY vbeln ASCENDING
                 posnr ASCENDING.

ENDFORM.                    " Z_SELECIONA_VBPA

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_RET                                          *
*&---------------------------------------------------------------------*
*                         Seleciona ZSDT_RETLOTE                       *
*----------------------------------------------------------------------*
FORM z_seleciona_ret.

  REFRESH t_ret.

  CHECK t_lin[] IS NOT INITIAL.

  SELECT docnum werks nf_retorno docnum_ret nfenum quant_vinc
    FROM zsdt_retlote
    INTO CORRESPONDING FIELDS OF TABLE t_ret
     FOR ALL ENTRIES IN t_lin
   WHERE docnum EQ t_lin-docnum.

  SORT t_ret BY docnum ASCENDING.

ENDFORM.                    " Z_SELECIONA_RET

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_AVINCULAR                                        *
*&---------------------------------------------------------------------*
*                        Monta Tabela a Vincular                       *
*----------------------------------------------------------------------*
FORM z_monta_avincular.

  DATA: sl_doc    TYPE type_doc,
        sl_correc TYPE type_correc,
        sl_lin    TYPE type_lin,
        sl_act    TYPE type_act,
        sl_avinc  TYPE type_avinc,
        sl_ret    TYPE ty_zsdt_retlote,
        vl_qtd    TYPE zsdt_retlote-quant_vinc,
***     Ch.129312 - Marcos Faneli
        wl_dco    TYPE ty_dco.

  SORT: t_dco     BY docnum.

  DATA: v_menge_avinc TYPE j_1bnflin-menge.

  REFRESH: t_avinc    ,
           t_avinc_aux.

  "CS2019001910 - Ini
  DATA(_mat_algodao) = abap_false.

  SELECT SINGLE *
    FROM mara INTO @DATA(wl_mara_inf)
  WHERE  matnr EQ @s_matnr-low.

  IF ( sy-subrc EQ 0 ) AND ( wl_mara_inf-matkl EQ '700140' ).
    _mat_algodao = abap_true.
  ENDIF.
  "CS2019001910 - Fim

  LOOP AT t_act INTO sl_act.

    v_index = sy-tabix.

    CLEAR: sl_avinc ,
           sl_doc   ,
           sl_ret   ,
           sl_correc,
           vl_qtd   .

    READ TABLE t_doc INTO sl_doc
      WITH KEY docnum = sl_act-docnum
      BINARY SEARCH.
*** Modificação - Eduardo Ruttkowski Tavares - 13.09.2013 >>> INI
* CH 109593 - AjusteZ_1BNFE_MONITOR_F32_ZSDI0001
    READ TABLE t_correc INTO sl_correc
      WITH KEY docnum = sl_doc-docnum
      BINARY SEARCH.
*** Modificação - Eduardo Ruttkowski Tavares - 13.09.2013 >>> INI

    sl_avinc-docnum = sl_doc-docnum.
    sl_avinc-credat = sl_doc-credat.
    sl_avinc-status = space.
    sl_avinc-nfenum = sl_doc-nfenum.

    LOOP AT t_lin INTO sl_lin
      WHERE docnum EQ sl_act-docnum.
      sl_lin-netwrt = sl_lin-netwr.
      ADD: sl_lin-menge  TO sl_avinc-menge ,
           sl_lin-netwrt TO sl_avinc-netwrt.

      sl_avinc-dtachegada = sl_lin-dtachegada.

      CLEAR sl_lin.
    ENDLOOP.

    LOOP AT t_ret INTO sl_ret
      "WHERE NFENUM EQ SL_AVINC-NFENUM. 07.12.2017 - Ini
       WHERE docnum EQ sl_avinc-docnum.

      ADD sl_ret-quant_vinc TO vl_qtd.
      CLEAR sl_ret.
    ENDLOOP.

    IF vl_qtd GE sl_avinc-menge.
      sl_avinc-saldo = 0.
      APPEND sl_avinc TO t_avinc_aux.
      CONTINUE.
    ELSE.

      "CS2019001910 - Fim
      IF _mat_algodao EQ abap_false.
        sl_avinc-saldo = trunc( sl_avinc-menge - vl_qtd ).
      ELSE.
        sl_avinc-saldo = sl_avinc-menge - vl_qtd.
      ENDIF.
      "CS2019001910 - Fim

      IF sl_avinc-saldo EQ 0.
        APPEND sl_avinc TO t_avinc_aux.
        CONTINUE.
      ENDIF.

      "DU-e 26.06.2018 - Ini
      IF it_rfl_zsdt0008[] IS NOT INITIAL.
        CLEAR: v_menge_avinc.
        LOOP AT it_rfl_zsdt0008 WHERE docnum_vinc = sl_avinc-docnum.
          ADD it_rfl_zsdt0008-peso_liq TO v_menge_avinc.
        ENDLOOP.

        IF sl_avinc-saldo < v_menge_avinc.
          CONTINUE.
        ELSE.
          sl_avinc-saldo = v_menge_avinc.
        ENDIF.
      ENDIF.
      "DU-e 26.06.2018 - Fim

    ENDIF.

*** Ch.192321 - Marcos Faneli
    IF s_dco_ IS NOT INITIAL.
      READ TABLE t_dco INTO wl_dco WITH KEY docnum = sl_avinc-docnum BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        APPEND sl_avinc TO t_avinc.
      ENDIF.
    ELSE.
      APPEND sl_avinc TO t_avinc.
    ENDIF.

    CLEAR sl_act.

  ENDLOOP.

  PERFORM check_chegada.

  SORT: t_avinc     BY dtachegada ASCENDING
                       nfenum ASCENDING,
        t_avinc_aux BY nfenum ASCENDING.

  t_avinc_bkp[] = t_avinc[].
  CLEAR: t_avinc[].

  "Incluir Registros com data de Chegada
  LOOP AT t_avinc_bkp WHERE dtachegada IS NOT INITIAL.
    APPEND t_avinc_bkp TO t_avinc.
  ENDLOOP.

  "Incluir Registros sem data de Chegada
  SORT t_avinc_bkp BY credat nfenum.
  LOOP AT t_avinc_bkp WHERE dtachegada IS INITIAL.
    APPEND t_avinc_bkp TO t_avinc.
  ENDLOOP.

ENDFORM.                    " Z_MONTA_AVINCULAR

*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      SET PF-STATUS 'PF0100'.
      SET TITLEBAR  'TB0100'.
    WHEN OTHERS.
  ENDCASE.

  DESCRIBE TABLE: t_avinc LINES tc_avinc-lines,
                  t_vinc  LINES tc_vinc-lines .


  IF p_tcode NE 'ZMEMO00'.
    LOOP AT SCREEN.
      IF screen-name EQ 'BTN_VINC_RFL_TERC'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " ZM_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              User Command                            *
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      CASE sy-ucomm.
        WHEN 'BACK' OR
             'CANC' OR
             'EXIT'.
          LEAVE TO SCREEN 0.
        WHEN 'BT_AUTO'.
*         Efetuar Vínculo Automático
          PERFORM z_vinc_auto.
        WHEN 'BT_MANU'.
*         Efetuar Vínculo Manual
          PERFORM z_vinc_manu.
        WHEN 'BT_REL'.
*         Impressão NF's Vinculadas
          PERFORM z_imprime_nfs.
        WHEN 'BT_DEL'.
*         Desfazer Vínculo
          PERFORM z_desf_vinc.
        WHEN 'BT_NF'.
*         Emitir NF
* US - 81360 - CBRAND - Inicio
          IF p_redex = 'X'.
            PERFORM z_emite_nf_redex.
          ELSE.
* US - 81360 - CBRAND - Inicio
            PERFORM z_emite_nf.
          ENDIF.
          LEAVE TO SCREEN 0.
        WHEN 'VINC_RFL_TERCEIRO'.

          CALL SCREEN 0101 STARTING AT 02 02.

        WHEN 'ORDER_QTDE_DESC'.
          SORT t_avinc BY saldo DESCENDING.
          LEAVE TO SCREEN 0100.
        WHEN 'ORDER_QTDE_ASC'.
          SORT t_avinc BY saldo.
          LEAVE TO SCREEN 0100.

        WHEN 'ORDER_DE_DESC'.
          SORT t_avinc BY credat DESCENDING.
          LEAVE TO SCREEN 0100.
        WHEN 'ORDER_DE_ASC'.
          SORT t_avinc BY credat.
          LEAVE TO SCREEN 0100.

        WHEN 'ORDER_DC_DESC'.
          SORT t_avinc BY dtachegada DESCENDING.
          LEAVE TO SCREEN 0100.
        WHEN 'ORDER_DC_ASC'.
          SORT t_avinc BY dtachegada.
          LEAVE TO SCREEN 0100.

      ENDCASE.
    WHEN OTHERS.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIN                                          *
*&---------------------------------------------------------------------*
*                        Seleciona J_1BNFLIN                           *
*----------------------------------------------------------------------*
FORM z_seleciona_lin.

  CHECK NOT t_vbrk[] IS INITIAL.

  SELECT l~docnum l~itmnum l~matnr l~maktx
         l~bwkey  l~matkl  l~nbm
         l~taxsit l~taxsi2 l~matuse
         l~refkey l~menge  l~meins
         l~netpr  l~netwr  l~netwrt
         l~taxlw1 l~taxlw2 l~itmtyp
         l~werks  l~taxlw3 l~taxlw4
    FROM j_1bnflin AS l
    APPENDING TABLE t_lin
    FOR ALL ENTRIES IN t_vbrk
  WHERE l~refkey EQ t_vbrk-refkey.
  "AND NOT EXISTS ( SELECT * FROM ZLEST0172 AS N WHERE N~DOCNUM EQ L~DOCNUM ).

ENDFORM.                    " Z_SELECIONA_LIN

*&---------------------------------------------------------------------*
*&      Module  ZM_AVINC_MARC  INPUT                                   *
*&---------------------------------------------------------------------*
*                        Marcação a Vincular                           *
*----------------------------------------------------------------------*
MODULE zm_avinc_marc INPUT.

  MODIFY t_avinc FROM s_avinc INDEX tc_avinc-current_line
    TRANSPORTING marc.

ENDMODULE.                 " ZM_AVINC_MARC  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_VINC_MARC  INPUT                                    *
*&---------------------------------------------------------------------*
*                           Marcação Vinculado                         *
*----------------------------------------------------------------------*
MODULE zm_vinc_marc INPUT.

  MODIFY t_vinc FROM s_vinc INDEX tc_vinc-current_line
    TRANSPORTING marc.

ENDMODULE.                 " ZM_VINC_MARC  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_VINC_AUTO                                              *
*&---------------------------------------------------------------------*
*                       Efetuar Vínculo Automático                     *
*----------------------------------------------------------------------*
FORM z_vinc_auto.

  DATA: tl_vinc  TYPE TABLE OF type_avinc,
        sl_avinc TYPE type_avinc,
        sl_vinc  TYPE type_avinc,
        vl_quant TYPE j_1bnflin-menge,
        vl_unit  TYPE j_1bnflin-netwrt,
        sl_ret   TYPE zsdt_retlote.

  vl_quant = p_quant - s_val-menge.

  IF s_val-menge GE p_quant.
    MESSAGE i836 WITH TEXT-011.
    EXIT.
  ENDIF.

  LOOP AT t_avinc INTO sl_avinc.

    v_index = sy-tabix.
    vl_unit = sl_avinc-netwrt / sl_avinc-menge.

    IF sl_avinc-saldo LT vl_quant.
      IF sl_avinc-saldo EQ sl_avinc-menge.
        sl_vinc = sl_avinc.
      ELSE.
        sl_vinc        = sl_avinc.
        sl_vinc-menge  = sl_avinc-saldo.
        sl_vinc-netwrt = sl_avinc-saldo * vl_unit.
      ENDIF.
      sl_vinc-marc   = space.
      sl_vinc-status = space.
      APPEND sl_vinc TO tl_vinc.
      ADD: sl_vinc-saldo  TO s_val-menge ,
           sl_vinc-netwrt TO s_val-netwrt.
      SUBTRACT sl_avinc-saldo FROM vl_quant.
      DELETE t_avinc INDEX v_index.
      READ TABLE t_avinc_aux
        "WITH KEY NFENUM = SL_AVINC-NFENUM "07.12.2017
        WITH KEY docnum = sl_avinc-docnum

        TRANSPORTING NO FIELDS.
      IF NOT sy-subrc IS INITIAL.
        APPEND sl_avinc TO t_avinc_aux.
      ENDIF.
    ELSE.
      sl_vinc = sl_avinc.
      sl_vinc-menge  = vl_quant.
      sl_vinc-netwrt = sl_vinc-menge * vl_unit.
      sl_vinc-status = space.
      sl_vinc-marc   = space.
      APPEND sl_vinc TO tl_vinc.
      ADD: sl_vinc-menge  TO s_val-menge ,
           sl_vinc-netwrt TO s_val-netwrt.
      sl_avinc-saldo = sl_avinc-saldo - vl_quant.
      sl_avinc-marc  = space.
      MODIFY t_avinc FROM sl_avinc
        INDEX v_index
        TRANSPORTING saldo marc.

      IF sl_avinc-saldo = 0.
        DELETE t_avinc INDEX v_index.
      ENDIF.

      EXIT.
    ENDIF.

    CLEAR: sl_avinc,
           sl_vinc ,
           vl_unit .

  ENDLOOP.

  IF t_vinc[] IS INITIAL.
    t_vinc[] = tl_vinc[].
  ELSE.
    SORT t_vinc BY nfenum ASCENDING.
    LOOP AT tl_vinc INTO sl_avinc.
      READ TABLE t_vinc INTO sl_vinc
        "WITH KEY NFENUM = SL_VINC-NFENUM. 07.12.2017
        WITH KEY docnum = sl_vinc-docnum.

      IF sy-subrc IS INITIAL.
        v_index = sy-tabix.
        ADD: sl_avinc-menge  TO sl_vinc-menge ,
             sl_avinc-netwrt TO sl_vinc-netwrt.
        MODIFY t_vinc FROM sl_vinc
          INDEX v_index
          TRANSPORTING menge netwrt.
      ELSE.
        APPEND sl_avinc TO t_vinc.
      ENDIF.
      CLEAR: sl_avinc,
             sl_vinc .
    ENDLOOP.
    SORT t_vinc BY dtachegada DESCENDING
                   nfenum ASCENDING.
  ENDIF.

ENDFORM.                    " Z_VINC_AUTO

*&---------------------------------------------------------------------*
*&      Form  Z_VINC_MANU                                              *
*&---------------------------------------------------------------------*
*                         Efetuar Vínculo Manual                       *
*----------------------------------------------------------------------*
FORM z_vinc_manu.

  DATA: tl_vinc  TYPE TABLE OF type_avinc,
        tl_avinc TYPE TABLE OF type_avinc,
        sl_avinc TYPE type_avinc,
        sl_vinc  TYPE type_avinc,
        vl_quant TYPE j_1bnflin-menge,
        vl_unit  TYPE j_1bnflin-netwrt,
        sl_ret   TYPE zsdt_retlote.

  vl_quant = p_quant - s_val-menge.

  IF s_val-menge GE p_quant.
    MESSAGE i836 WITH TEXT-011.
    EXIT.
  ENDIF.

  tl_avinc[] = t_avinc[].
  DELETE tl_avinc WHERE marc IS INITIAL.
  IF tl_avinc[] IS INITIAL.
    MESSAGE i836 WITH TEXT-010.
    EXIT.
  ENDIF.

  LOOP AT t_avinc INTO sl_avinc
    WHERE marc NE space.

    v_index = sy-tabix.
    vl_unit = sl_avinc-netwrt / sl_avinc-menge.

    IF sl_avinc-saldo LT vl_quant.
      IF sl_avinc-saldo EQ sl_avinc-menge.
        sl_vinc = sl_avinc.
      ELSE.
        sl_vinc = sl_avinc.
        sl_vinc-menge  = sl_avinc-saldo.
        sl_vinc-netwrt = sl_avinc-saldo * vl_unit.
      ENDIF.
      sl_vinc-marc = space.
      sl_vinc-status = space.
      APPEND sl_vinc TO tl_vinc.
      ADD: sl_vinc-saldo  TO s_val-menge ,
           sl_vinc-netwrt TO s_val-netwrt.
      SUBTRACT sl_avinc-saldo FROM vl_quant.
      DELETE t_avinc INDEX v_index.

      READ TABLE t_avinc_aux
        "WITH KEY NFENUM = SL_AVINC-NFENUM 07.12.2017
        WITH KEY docnum = sl_avinc-docnum

        TRANSPORTING NO FIELDS.
      IF NOT sy-subrc IS INITIAL.
        APPEND sl_avinc TO t_avinc_aux.
      ENDIF.
    ELSE.
      sl_vinc        = sl_avinc.
      sl_vinc-menge  = vl_quant.
      sl_vinc-netwrt = sl_vinc-menge * vl_unit.
      sl_vinc-marc   = space.
      sl_vinc-status = space.
      APPEND sl_vinc TO tl_vinc.
      ADD: sl_vinc-menge  TO s_val-menge ,
           sl_vinc-netwrt TO s_val-netwrt.
      sl_avinc-saldo = sl_avinc-saldo - vl_quant.
      sl_avinc-marc  = space.
      MODIFY t_avinc FROM sl_avinc
        INDEX v_index
        TRANSPORTING saldo marc.
      EXIT.
    ENDIF.

    CLEAR: sl_avinc,
           sl_vinc ,
           vl_unit .

  ENDLOOP.

  IF t_vinc[] IS INITIAL.
    t_vinc[] = tl_vinc[].
  ELSE.
    SORT t_vinc BY nfenum ASCENDING.
    LOOP AT tl_vinc INTO sl_avinc.
      READ TABLE t_vinc INTO sl_vinc
        "WITH KEY NFENUM = SL_VINC-NFENUM. 07.12.2017
        WITH KEY docnum = sl_vinc-docnum.

      IF sy-subrc IS INITIAL.
        v_index = sy-tabix.
        ADD: sl_avinc-menge  TO sl_vinc-menge ,
             sl_avinc-netwrt TO sl_vinc-netwrt.
        MODIFY t_vinc FROM sl_vinc
          INDEX v_index
          TRANSPORTING menge netwrt.
      ELSE.
        APPEND sl_avinc TO t_vinc.
      ENDIF.
      CLEAR: sl_avinc,
             sl_vinc .
    ENDLOOP.
    SORT t_vinc BY dtachegada DESCENDING
                   nfenum ASCENDING.
  ENDIF.

ENDFORM.                    " Z_VINC_MANU

*&---------------------------------------------------------------------*
*&      Form  Z_DESF_VINC                                              *
*&---------------------------------------------------------------------*
*                           Desfazer Vínculo                           *
*----------------------------------------------------------------------*
FORM z_desf_vinc.

  DATA: tl_vinc  TYPE TABLE OF type_avinc,
        tl_avinc TYPE TABLE OF type_avinc,
        sl_avinc TYPE type_avinc,
        sl_vinc  TYPE type_avinc,
        vl_quant TYPE j_1bnflin-menge.

  tl_vinc[] = t_vinc[].
  DELETE tl_vinc WHERE marc IS INITIAL.
  IF tl_vinc[] IS INITIAL.
    MESSAGE i836 WITH TEXT-013.
    EXIT.
  ENDIF.

  SORT t_avinc_aux BY nfenum ASCENDING.

  LOOP AT t_vinc INTO sl_vinc
    WHERE marc NE space.

    v_index = sy-tabix.

    CHECK sl_vinc-status IS INITIAL.

    sl_avinc       = sl_vinc.
    sl_avinc-saldo = sl_vinc-menge.
    sl_avinc-marc  = space.
    APPEND sl_avinc TO tl_avinc.
    SUBTRACT: sl_vinc-menge  FROM s_val-menge ,
              sl_vinc-netwrt FROM s_val-netwrt.

    DELETE t_vinc INDEX v_index.

    CLEAR: sl_vinc ,
           sl_avinc.

  ENDLOOP.

  SORT t_avinc BY nfenum ASCENDING.

  LOOP AT tl_avinc INTO sl_vinc.
*
*    READ TABLE T_AVINC INTO SL_AVINC
*      WITH KEY NFENUM = SL_VINC-NFENUM
*      BINARY SEARCH.

    TRY .
        "SL_AVINC = T_AVINC[ NFENUM = SL_VINC-NFENUM ].                  07.12.2017
        "V_INDEX = LINE_INDEX( T_AVINC[ NFENUM = SL_VINC-NFENUM ] ).     07.12.2017

        sl_avinc = t_avinc[ docnum = sl_vinc-docnum ].
        v_index = line_index( t_avinc[ docnum = sl_vinc-docnum ] ).

      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    IF NOT sl_avinc IS INITIAL.

      ADD sl_vinc-saldo TO sl_avinc-saldo.
      MODIFY t_avinc FROM sl_avinc
        INDEX v_index
        TRANSPORTING saldo.

    ELSE.
      TRY .
          "SL_AVINC = T_AVINC_AUX[ NFENUM = SL_VINC-NFENUM ].  07.12.2017
          sl_avinc = t_avinc_aux[ docnum = sl_vinc-docnum ].

        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      sl_avinc-saldo = sl_vinc-saldo.
      sl_avinc-marc  = space.
      APPEND sl_avinc TO t_avinc.

    ENDIF.

*    IF SY-SUBRC IS INITIAL.
*      V_INDEX = SY-TABIX.
*      ADD SL_VINC-SALDO TO SL_AVINC-SALDO.
*      MODIFY T_AVINC FROM SL_AVINC
*        INDEX V_INDEX
*        TRANSPORTING SALDO.
*    ELSE.
*      READ TABLE T_AVINC_AUX INTO SL_AVINC
*        WITH KEY NFENUM = SL_VINC-NFENUM
*        BINARY SEARCH.
*      SL_AVINC-SALDO = SL_VINC-SALDO.
*      SL_AVINC-MARC  = SPACE.
*      APPEND SL_AVINC TO T_AVINC.
*    ENDIF.

    CLEAR: sl_vinc ,
           sl_avinc.

  ENDLOOP.

  SORT t_avinc BY dtachegada DESCENDING
                  nfenum ASCENDING.

ENDFORM.                    " Z_DESF_VINC

*&---------------------------------------------------------------------*
*&      Form  Z_EMITE_NF                                               *
*&---------------------------------------------------------------------*
*                                Emitir NF                             *
*----------------------------------------------------------------------*
FORM z_emite_nf.

  DATA: sl_header     TYPE bapi_j_1bnfdoc,
        sl_header_add TYPE bapi_j_1bnfdoc_add,
        tl_partner    TYPE TABLE OF bapi_j_1bnfnad,
        tl_item       TYPE TABLE OF bapi_j_1bnflin,
        tl_item_add   TYPE TABLE OF bapi_j_1bnflin_add,
        tl_item_tax   TYPE TABLE OF bapi_j_1bnfstx,
        tl_return     TYPE TABLE OF bapiret2,
        tl_ret        TYPE TABLE OF zsdt_retlote,
        sl_doc        TYPE type_doc,
        tl_msg        TYPE TABLE OF bapi_j_1bnfftx,
        sl_lin        TYPE type_lin,
        sl_vinc       TYPE type_avinc,
        sl_partner    TYPE bapi_j_1bnfnad,
        sl_item       TYPE bapi_j_1bnflin,
        sl_item_add   TYPE bapi_j_1bnflin_add,
        sl_ret        TYPE zsdt_retlote,
        vl_itmnum     TYPE j_1bnflin-itmnum,
        vl_answer     TYPE char1,
        vl_cfop       TYPE j_1bcfop,
        vl_docnum     TYPE j_1bnfdoc-docnum,
        docnum        TYPE j_1bnfdoc-docnum,
        vl_nfenum     TYPE j_1bnfdoc-nfenum,
        vl_qtd        TYPE j_1bnetqty,
        vl_netwrt     TYPE j_1bnflin-netwrt,
        vl_lifnr      TYPE vbpa-lifnr,
        sl_nfcheck    TYPE bapi_j_1bnfcheck,
        sl_export     TYPE zsdt_export,
        wa_j_1baa     TYPE j_1baa,
        wa_j_1bb2     TYPE j_1bb2,
        tg_lin_aux    TYPE TABLE OF j_1bnflin WITH HEADER LINE,
        vl_qtde_nota  TYPE j_1bnflin-menge,
        vl_qtde_vinc  TYPE j_1bnflin-menge,
        vl_qtde_dif   TYPE j_1bnflin-menge,
        vl_menge_aux  TYPE j_1bnflin-menge.
  DATA(_error) = ''.

*** US - 81360 - Inicio - CBRAND
  LOOP AT t_vinc INTO sl_vinc.

*** US - 81360 - Inicio - CBRAND
*** Mudamos essa parte do codigo abaixo e criamos o metodo novo nessa classe.
    TRY.
        zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance(
                                  )->validar_quantidade_retorno( EXPORTING i_docnum     = sl_vinc-docnum
                                                                           i_menge_vinc = sl_vinc-menge ).

      CATCH zcx_controle_retorno_rfl INTO DATA(zcx_controle_rfl).
        zcx_controle_rfl->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).
        RETURN.
    ENDTRY.
*** US - 81360 - Fim - CBRAND


*    SELECT SUM( quant_vinc )
*      FROM zsdt_retlote INTO vl_qtde_vinc
*     WHERE docnum = sl_vinc-docnum.
*
*    CLEAR: tg_lin_aux[].
*    SELECT *
*      FROM j_1bnflin INTO TABLE tg_lin_aux
*     WHERE docnum = sl_vinc-docnum.
*
*    IF tg_lin_aux[] IS INITIAL.
*      MESSAGE i836 WITH text-032.
*      RETURN.
*    ENDIF.
*
*    LOOP AT tg_lin_aux.
*
*      IF tg_lin_aux-meins NE 'KG'.
*
*        CLEAR: vl_menge_aux.
*
*        CALL FUNCTION 'ME_CONVERSION_MEINS'
*          EXPORTING
*            i_matnr             = tg_lin_aux-matnr
*            i_mein1             = tg_lin_aux-meins
*            i_meins             = 'KG'
*            i_menge             = tg_lin_aux-menge
*          IMPORTING
*            menge               = vl_menge_aux
*          EXCEPTIONS
*            error_in_conversion = 1
*            no_success          = 2
*            OTHERS              = 3.
*
*        IF sy-subrc NE 0.
*          MESSAGE |Erro conversão quantidade documento { sl_vinc-docnum }! | TYPE 'I'.
*          RETURN.
*        ENDIF.
*
*        ADD vl_menge_aux TO vl_qtde_nota.
*      ELSE.
*        ADD tg_lin_aux-menge TO vl_qtde_nota.
*      ENDIF.
*    ENDLOOP.
*    IF ( vl_qtde_vinc + sl_vinc-menge ) > vl_qtde_nota.
*      vl_qtde_dif = ( vl_qtde_vinc + sl_vinc-menge ) - vl_qtde_nota.
*      MESSAGE |Quantidade do documento { sl_vinc-docnum } excedida em { vl_qtde_dif } ! | TYPE 'I'.
*      RETURN.
*    ENDIF.
  ENDLOOP.
*** US - 81360 - Fim - CBRAND

  IF s_val-menge NE p_quant.
    MESSAGE i836 WITH TEXT-015.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question  = TEXT-014
    IMPORTING
      answer         = vl_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CASE vl_answer.
    WHEN '2' OR
         'A'.
      EXIT.
  ENDCASE.

  READ TABLE t_vinc INTO sl_vinc INDEX 1.
  READ TABLE t_doc INTO sl_doc
    WITH KEY docnum = sl_vinc-docnum
    BINARY SEARCH.

  IF s_nf_terc IS NOT INITIAL. "Lançamento Terceiro
    DATA(_nf_type) = 'ZD'.

    IF p_depfec IS NOT INITIAL.
      _nf_type = 'ZH'.
    ENDIF.
  ELSE.
    _nf_type = 'ZV'.
  ENDIF.

  SELECT SINGLE * INTO wa_j_1baa
    FROM j_1baa
   WHERE nftype EQ _nf_type.

  IF sy-subrc NE 0.
    MESSAGE i836 WITH TEXT-015 _nf_type.
    EXIT.
  ENDIF.
  "Lançamento terceiro
  CHECK ( wa_j_1baa-form IS NOT INITIAL ) OR ( s_nf_terc IS NOT INITIAL ).

  CLEAR sl_header-series.

  IF sy-subrc IS INITIAL.

    IF wa_j_1baa-form IS NOT INITIAL.
      SELECT SINGLE * INTO wa_j_1bb2
        FROM j_1bb2
       WHERE bukrs  EQ sl_doc-bukrs
         AND branch EQ sl_doc-branch
         AND form   EQ wa_j_1baa-form.

      IF sy-subrc IS INITIAL.
        MOVE wa_j_1bb2-series TO sl_header-series.
      ENDIF.
    ELSE.
      MOVE p_series TO sl_header-series.
    ENDIF.

  ENDIF.

  CHECK NOT sl_header-series IS INITIAL.
  MOVE-CORRESPONDING wa_j_1baa TO sl_header.
* Preenchimenyto Header
  "sl_header-nftype  = 'ZL'.
  "sl_header-doctyp  = 1.
  "sl_header-direct  = 1.
  IF s_nf_terc IS NOT INITIAL. "Lançamento Terceiro
    sl_header-docdat = p_docdat.
  ELSE.
    sl_header-docdat = sy-datum.
  ENDIF.
  sl_header-pstdat  = sy-datum.
  sl_header-credat  = sy-datum.
  "sl_header-model   = 55.
  "sl_header-series  = 0.
  "sl_header-nfnum   = 1.

  IF wa_j_1baa-form IS INITIAL.
    sl_header-nfenum  = p_nfenum.
    sl_header-series  = p_series.

    IF ( sl_header-nfenum IS INITIAL ) OR
       ( sl_header-series IS INITIAL ).
      MESSAGE i836 WITH TEXT-028.
      EXIT.
    ENDIF.
  ENDIF.

  CONDENSE sl_header-series.
  sl_header-manual  = 'X'.
  sl_header-waerk   = 'BRL'.
  sl_header-bukrs   = sl_doc-bukrs.
  sl_header-branch  = sl_doc-branch.
  sl_header-parvw   = 'AG'.
  sl_header-parid   = sl_doc-kunnr.
  sl_header-partyp  = 'C'.

  IF ( p_depfec IS NOT INITIAL ) AND ( s_nf_terc IS NOT INITIAL ). "Lançamento Terceiro.
    sl_header-partyp  = sl_doc-partyp.
    sl_header-parvw   = sl_doc-parvw.
  ENDIF.

  "sl_header-nfe     = 'X'.

* Preenche Header ADD
  sl_header_add-nftot = s_val-netwrt.

* Preenche NFCHECK
  sl_nfcheck-chekcon = 'X'.

* Preenche Partner
  sl_partner-parvw  = 'AG'.
  sl_partner-parid  = sl_doc-kunnr.
  sl_partner-partyp = 'C'.

  IF ( p_depfec IS NOT INITIAL ) AND ( s_nf_terc IS NOT INITIAL ). "Lançamento Terceiro.
    sl_partner-partyp  = sl_doc-partyp.
    sl_partner-parvw   = sl_doc-parvw.
  ENDIF.

  APPEND sl_partner TO tl_partner.

  sl_partner-parvw  = 'Z1'.
  sl_partner-parid  = s_parc-low.
  sl_partner-partyp = 'V'.
  APPEND sl_partner TO tl_partner.

  READ TABLE t_lin INTO sl_lin
    WITH KEY docnum = sl_vinc-docnum
    BINARY SEARCH.

* Determinar CFOP
  PERFORM z_determinar_cfop USING s_matnr-low
                                  sl_doc-bukrs
                                  sl_doc-branch
                                  sl_doc-kunnr
                                  sl_lin-refkey
                         CHANGING vl_cfop
                                  vl_lifnr.

* Preenche Partner LF
  IF NOT vl_lifnr IS INITIAL.
    sl_partner-parvw  = 'LF'.
    sl_partner-parid  = vl_lifnr.
    sl_partner-partyp = 'V'.
    APPEND sl_partner TO tl_partner.
  ENDIF.

* Preenche Item
  ADD 10 TO vl_itmnum.
  sl_item-itmnum  = vl_itmnum.

* ---> S4 Migration - 06/07/2023 - FC
  "sl_item-matnr   = sl_lin-matnr.

  DATA(v_len) = strlen( sl_lin-matnr ).

  IF v_len > 18.
    sl_item-matnr_long = sl_lin-matnr.
  ELSE.
    sl_item-matnr = sl_lin-matnr.
  ENDIF.
* <--- S4 Migration - 06/07/2023 - FC

  sl_item-maktx   = sl_lin-maktx.
  sl_item-bwkey   = sl_lin-bwkey.
  sl_item-matkl   = sl_lin-matkl.
  sl_item-nbm     = sl_lin-nbm.
  sl_item-charg   = s_safra-low.   "Lote
*  sl_item-taxsit  = sl_lin-taxsit.
  sl_item-taxsit  = '50'.
  sl_item-taxsi2  = sl_lin-taxsi2.
  sl_item-matuse  = sl_lin-matuse.
  sl_item-menge   = s_val-menge.
  sl_item-meins   = sl_lin-meins.
  sl_item-itmtyp  = sl_lin-itmtyp.
  sl_item-werks   = sl_lin-werks.
  sl_item-cfop_10 = vl_cfop.
  sl_item-netpr   = s_val-netwrt / s_val-menge.
  sl_item-netwr   = s_val-netwrt.
  sl_item-matorg  = '0'.
  sl_item-taxlw1  = 'IC5'.
  sl_item-taxlw2  = 'I03'.
  sl_item-taxlw4  = 'C08'.
  sl_item-taxlw5  = 'P08'.
  IF ( p_depfec IS NOT INITIAL ) AND ( s_nf_terc IS NOT INITIAL ). "Lançamento Terceiro.
    sl_item-cfop_10 = '1906AA'.
    sl_item-itmtyp  = 'W0'.
    sl_item-taxlw1  = 'C42'.
  ENDIF.
  APPEND sl_item TO tl_item.

* Preenche Item ADD
  sl_item_add-itmnum = vl_itmnum.
  sl_item_add-direct = 1.
  APPEND sl_item_add TO tl_item_add.

* Preenche Item TAX
  PERFORM z_preenche_tax TABLES tl_item_tax
                          USING: 'IPIS' vl_itmnum,
                                 'IPI3' vl_itmnum,
                                 'ICM3' vl_itmnum,
                                 'ICOF' vl_itmnum.

* Preenche OBJ_HEADER_MSG
  PERFORM z_prenche_msg TABLES tl_msg
                           USING: 1 TEXT-017,
                                  2 TEXT-018.
* Cria NF
  CALL FUNCTION 'BAPI_J_1B_NF_CREATEFROMDATA' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      obj_header     = sl_header
      obj_header_add = sl_header_add
      nfcheck        = sl_nfcheck
    IMPORTING
      e_docnum       = vl_docnum
    TABLES
      obj_partner    = tl_partner
      obj_item       = tl_item
      obj_item_add   = tl_item_add
      obj_item_tax   = tl_item_tax
      obj_header_msg = tl_msg
      return         = tl_return.



  IF vl_docnum IS INITIAL.
*   Retorna Erros da BAPI
    PERFORM z_ret_erros TABLES tl_return.
  ELSE.

    LOOP AT t_vinc INTO sl_vinc.
      sl_ret-nfenum       = sl_vinc-nfenum.
      sl_ret-werks        = p_werks.
      sl_ret-nf_retorno   = space.
      sl_ret-data_criacao = sy-datum.
      sl_ret-data_saida   = sl_vinc-credat.
      sl_ret-quant_vinc   = sl_vinc-menge.
      sl_ret-vlr_total    = sl_vinc-netwrt.
      sl_ret-status       = 'A'.
      sl_ret-docnum       = sl_vinc-docnum.
      sl_ret-docnum_ret   = vl_docnum.
      sl_ret-bukrs        = p_bukrs.
      sl_ret-safra        = s_safra-low.
      sl_ret-lgort        = s_lgort-low.
      sl_ret-matnr        = s_matnr-low.
      sl_ret-parceiro     = s_parc-low.
      sl_ret-unitario     = sl_vinc-netwrt / sl_vinc-menge.
      ADD: sl_vinc-menge  TO vl_qtd   ,
           sl_vinc-netwrt TO vl_netwrt.
      APPEND sl_ret TO tl_ret.
      CLEAR: sl_vinc,
             sl_ret .
    ENDLOOP.

    READ TABLE t_vinc INTO sl_vinc INDEX 1.
    sl_export-docnum       = vl_docnum.
    sl_export-nf_retorno   = space.
    sl_export-werks        = p_werks.
    sl_export-data_criacao = sy-datum.
    sl_export-quant        = vl_qtd.
    sl_export-matnr        = s_matnr-low.
    sl_export-valor_total  = vl_netwrt.
    sl_export-export       = space.
    sl_export-status       = 'A'.
    sl_export-finalidade   = p_final.

    IF s_nf_terc IS NOT INITIAL. "Lançamento Terceiro
      sl_export-nf_retorno = p_nfenum.
      sl_export-status     = 'X'.
    ENDIF.

    LOOP AT it_ret_ter ASSIGNING FIELD-SYMBOL(<fs_ret_ter>).
      <fs_ret_ter>-docnum_ret = vl_docnum.
    ENDLOOP.

    IF NOT tl_ret[] IS INITIAL.
      INSERT zsdt_retlote FROM TABLE tl_ret.
      IF sy-subrc NE 0.
        _error = 'X'.
      ENDIF.
    ENDIF.

    IF NOT sl_export IS INITIAL.
      MODIFY zsdt_export FROM sl_export.
      IF sy-subrc NE 0.
        _error = 'X'.
      ENDIF.
    ENDIF.

    IF it_ret_ter[] IS NOT INITIAL.
      INSERT zsdt_retlote_ter FROM TABLE it_ret_ter.
      IF sy-subrc NE 0.
        _error = 'X'.
      ENDIF.
    ENDIF.

    IF _error IS INITIAL.
      IF s_nf_terc IS NOT INITIAL. "Lançamento Terceiro
        CLEAR: p_nfenum, p_series,p_docdat,p_netwr.
      ENDIF.

      MESSAGE i836 WITH TEXT-016 vl_docnum.
      SET PARAMETER ID 'JEF' FIELD vl_docnum.

      CASE sy-tcode.
        WHEN 'ZSDT0066'.
          docnum = vl_docnum.
          EXPORT: docnum TO MEMORY ID 'ZRET'.
      ENDCASE.

      IF p_tcode EQ 'ZSDT0066'.
        docnum = vl_docnum.
        EXPORT: docnum TO MEMORY ID 'ZRET'.
      ENDIF.

      IF ( s_nf_terc IS NOT INITIAL ). "Lançamento Terceiro.
        CALL FUNCTION 'Z_INFO_NFE_FORNECEDOR_GERAL_01'
          EXPORTING
            i_docnum = vl_docnum.
      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.


*CS2022000854 Automatização Estorno Trânsito da Mercadoria / Anderson Oenning
      "Setar estorno transito caso finalidade atenda as condições abaixo.
      PERFORM zf_seleciona_tvarv.
      READ TABLE rg_final INTO DATA(rw_finl) WITH KEY low = sl_export-finalidade.
      IF sy-subrc EQ 0.
        READ TABLE tl_ret INTO DATA(ws_ret) WITH KEY docnum_ret = sl_export-docnum.
        IF sy-subrc EQ 0.
          READ TABLE it_zlest0039 INTO ws_zlest0039 WITH KEY docnum  = ws_ret-docnum.
          IF sy-subrc EQ 0.
            UPDATE zlest0039
              SET ck_estornar_trans = abap_true
            WHERE docnum EQ ws_ret-docnum.
          ENDIF.
        ENDIF.
      ENDIF.
*CS2022000854 Automatização Estorno Trânsito da Mercadoria / Anderson Oenning
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      MESSAGE e836 WITH TEXT-029 TEXT-031 vl_docnum.
    ENDIF.

  ENDIF.

ENDFORM.                    " Z_EMITE_NF

*&---------------------------------------------------------------------*
*&      Form  Z_EMITE_NF_REDEX                                         *
*&---------------------------------------------------------------------*
*                                Emitir NF  REDEX                      *
*----------------------------------------------------------------------*
FORM z_emite_nf_redex.
*** US - 81360 - Inicio - CBRAND
  DATA: sl_header     TYPE bapi_j_1bnfdoc,
        sl_header_add TYPE bapi_j_1bnfdoc_add,
        tl_partner    TYPE TABLE OF bapi_j_1bnfnad,
        tl_item       TYPE TABLE OF bapi_j_1bnflin,
        tl_item_add   TYPE TABLE OF bapi_j_1bnflin_add,
        tl_item_tax   TYPE TABLE OF bapi_j_1bnfstx,
        tl_return     TYPE TABLE OF bapiret2,
        tl_ret        TYPE TABLE OF zsdt_retlote,
        sl_doc        TYPE type_doc,
        tl_msg        TYPE TABLE OF bapi_j_1bnfftx,
        sl_lin        TYPE type_lin,
        sl_vinc       TYPE type_avinc,
        sl_partner    TYPE bapi_j_1bnfnad,
        sl_item       TYPE bapi_j_1bnflin,
        sl_item_add   TYPE bapi_j_1bnflin_add,
        sl_ret        TYPE zsdt_retlote,
        vl_itmnum     TYPE j_1bnflin-itmnum,
        vl_answer     TYPE char1,
        vl_cfop       TYPE j_1bcfop,
        vl_docnum     TYPE j_1bnfdoc-docnum,
        docnum        TYPE j_1bnfdoc-docnum,
        id_redex      TYPE zsdt_export-id_export,
        vl_nfenum     TYPE j_1bnfdoc-nfenum,
        vl_qtd        TYPE j_1bnetqty,
        vl_netwrt     TYPE j_1bnflin-netwrt,
        vl_lifnr      TYPE vbpa-lifnr,
        sl_nfcheck    TYPE bapi_j_1bnfcheck,
        sl_export     TYPE zsdt_export,
        wa_j_1baa     TYPE j_1baa,
        wa_j_1bb2     TYPE j_1bb2,
        tg_lin_aux    TYPE TABLE OF j_1bnflin WITH HEADER LINE,
        vl_qtde_nota  TYPE j_1bnflin-menge,
        vl_qtde_vinc  TYPE j_1bnflin-menge,
        vl_qtde_dif   TYPE j_1bnflin-menge,
        vl_menge_aux  TYPE j_1bnflin-menge.

  DATA(_error) = ''.
  LOOP AT t_vinc INTO sl_vinc.
    TRY.
        zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance(
                                  )->validar_quantidade_retorno( EXPORTING i_docnum     = sl_vinc-docnum
                                                                           i_menge_vinc = sl_vinc-menge ).

      CATCH zcx_controle_retorno_rfl INTO DATA(zcx_controle_rfl).
        zcx_controle_rfl->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).
        RETURN.
    ENDTRY.
  ENDLOOP.

  IF s_val-menge NE p_quant.
    MESSAGE i836 WITH TEXT-015.
    EXIT.
  ENDIF.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question  = TEXT-014
    IMPORTING
      answer         = vl_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CASE vl_answer.
    WHEN '2' OR
         'A'.
      EXIT.
  ENDCASE.

  CALL METHOD zcl_fiscal=>get_documento_chave
    EXPORTING
      i_chave   = p_chvnfe
      i_propria = abap_false
    RECEIVING
      r_docnum  = vl_docnum
    EXCEPTIONS
      erro      = 1
      OTHERS    = 2.


  READ TABLE t_vinc INTO sl_vinc INDEX 1.

  READ TABLE t_doc INTO sl_doc
    WITH KEY docnum = sl_vinc-docnum
    BINARY SEARCH.

  IF vl_docnum IS INITIAL.

    DATA(_nf_type) =  'YF'.

    SELECT SINGLE * INTO wa_j_1baa
      FROM j_1baa
     WHERE nftype EQ _nf_type.

    IF sy-subrc NE 0.
      MESSAGE i836 WITH TEXT-015 _nf_type.
      EXIT.
    ENDIF.

    CLEAR sl_header-series.

    READ TABLE t_zib_nfe_dist_ter INTO DATA(lwa_zib_nfe_dist_ter) INDEX 1.

    sl_header-series  = lwa_zib_nfe_dist_ter-serie.

    CHECK NOT sl_header-series IS INITIAL.
    MOVE-CORRESPONDING wa_j_1baa TO sl_header.

    sl_header-docdat  = lwa_zib_nfe_dist_ter-dt_emissao.
    sl_header-pstdat  = sy-datum.
    sl_header-credat  = sy-datum.
    sl_header-access_key  = lwa_zib_nfe_dist_ter-chave_nfe.
    sl_header-docstat     = '1'.
    sl_header-tpemis      = lwa_zib_nfe_dist_ter-cd_form_emissao.
    sl_header-nfenum      = lwa_zib_nfe_dist_ter-numero.
    sl_header-series      = lwa_zib_nfe_dist_ter-serie.

    IF ( sl_header-nfenum IS INITIAL ) OR
       ( sl_header-series IS INITIAL ).
      MESSAGE i836 WITH TEXT-028.
      EXIT.
    ENDIF.

    CONDENSE sl_header-series.
    sl_header-manual  = 'X'.
    sl_header-waerk   = 'BRL'.
    sl_header-bukrs   = sl_doc-bukrs.
    sl_header-branch  = sl_doc-branch.
    sl_header-parvw   = 'AG'.
    sl_header-parid   = sl_doc-kunnr.
    sl_header-partyp  = 'C'.

* Preenche Header ADD
    sl_header_add-nftot = lwa_zib_nfe_dist_ter-vl_total_fatura.
    s_val-nftot  = lwa_zib_nfe_dist_ter-vl_total_fatura.

* Preenche NFCHECK
    sl_nfcheck-chekcon = 'X'.

* Preenche Partner
    sl_partner-parvw  = 'AG'.
    sl_partner-parid  = sl_doc-kunnr.
    sl_partner-partyp = 'C'.
    APPEND sl_partner TO tl_partner.

    sl_partner-parvw  = 'Z1'.
    sl_partner-parid  = s_parc-low.
    sl_partner-partyp = 'V'.
    APPEND sl_partner TO tl_partner.

    READ TABLE t_lin INTO sl_lin
      WITH KEY docnum = sl_vinc-docnum
      BINARY SEARCH.

* Determinar CFOP
    PERFORM z_determinar_cfop USING s_matnr-low
                                    sl_doc-bukrs
                                    sl_doc-branch
                                    sl_doc-kunnr
                                    sl_lin-refkey
                           CHANGING vl_cfop
                                    vl_lifnr.

* Preenche Partner LF
    IF NOT vl_lifnr IS INITIAL.
      sl_partner-parvw  = 'LF'.
      sl_partner-parid  = vl_lifnr.
      sl_partner-partyp = 'V'.
      APPEND sl_partner TO tl_partner.
    ENDIF.

* Preenche Item

    SORT t_zib_nfe_dist_itm BY chave_nfe prod_item.

    READ TABLE t_zib_nfe_dist_itm INTO DATA(lwa_zib_nfe_dist_itm)
    WITH KEY   chave_nfe = lwa_zib_nfe_dist_ter-chave_nfe.


    ADD 10 TO vl_itmnum.
    sl_item-itmnum  = vl_itmnum.

* ---> S4 Migration - 06/07/2023 - FC
    "sl_item-matnr   = sl_lin-matnr.

    DATA(v_len) = strlen( sl_lin-matnr ).

    IF v_len > 18.
      sl_item-matnr_long = sl_lin-matnr.
    ELSE.
      sl_item-matnr = sl_lin-matnr.
    ENDIF.
* <--- S4 Migration - 06/07/2023 - FC

    sl_item-maktx   = sl_lin-maktx.
    sl_item-bwkey   = sl_lin-bwkey.
    sl_item-matkl   = sl_lin-matkl.
    sl_item-nbm     = sl_lin-nbm.
    sl_item-charg   = s_safra-low.   "Lote
    sl_item-taxsit  = '50'.
    sl_item-taxsi2  = '03'.
    sl_item-matuse  = '1'.
    sl_item-menge   = lwa_zib_nfe_dist_itm-prod_qtd_comerci.
    sl_item-meins   = sl_lin-meins.
    sl_item-itmtyp  = '1'.
    sl_item-werks   = sl_lin-werks.
    sl_item-cfop_10 = '2949AA'.
    sl_item-netpr   = lwa_zib_nfe_dist_itm-prod_qtd_comerci .
    sl_item-netwr   = lwa_zib_nfe_dist_itm-prod_vlr_total_b.
    sl_item-matorg  = '0'.
    sl_item-taxlw1  = 'IC5'.
    sl_item-taxlw2  = 'I03'.
    sl_item-taxlw4  = 'C08'.
    sl_item-taxlw5  = 'P08'.

    APPEND sl_item TO tl_item.

* Preenche Item ADD
    sl_item_add-itmnum = vl_itmnum."lwa_zib_nfe_dist_itm-prod_item.
    sl_item_add-direct = 1.
    APPEND sl_item_add TO tl_item_add.

* Preenche Item TAX
    PERFORM z_preenche_tax TABLES tl_item_tax
                            USING: 'IPIS' vl_itmnum,
                                   'IPI3' vl_itmnum,
                                   'ICM3' vl_itmnum,
                                   'ICOF' vl_itmnum.

* Cria NF
    CALL FUNCTION 'BAPI_J_1B_NF_CREATEFROMDATA' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        obj_header     = sl_header
        obj_header_add = sl_header_add
        nfcheck        = sl_nfcheck
      IMPORTING
        e_docnum       = vl_docnum
      TABLES
        obj_partner    = tl_partner
        obj_item       = tl_item
        obj_item_add   = tl_item_add
        obj_item_tax   = tl_item_tax
        obj_header_msg = tl_msg
        return         = tl_return.

  ENDIF.

  IF vl_docnum IS INITIAL.
*   Retorna Erros da BAPI
    PERFORM z_ret_erros TABLES tl_return.
  ELSE.

    PERFORM get_next_number  USING  'ZID_REDEX'
                  '01'
         CHANGING sl_export-id_export.

    LOOP AT t_vinc INTO sl_vinc.
      sl_ret-nfenum       = sl_vinc-nfenum.
      sl_ret-werks        = p_werks.
      sl_ret-nf_retorno   = space.
      sl_ret-data_criacao = sy-datum.
      sl_ret-data_saida   = sl_vinc-credat.
      sl_ret-quant_vinc   = sl_vinc-menge.
      sl_ret-vlr_total    = sl_vinc-netwrt.
      sl_ret-status       = 'A'.
      sl_ret-docnum       = sl_vinc-docnum.
      sl_ret-docnum_ret   = vl_docnum.
      sl_ret-bukrs        = p_bukrs.
      sl_ret-safra        = s_safra-low.
      sl_ret-lgort        = s_lgort-low.
      sl_ret-matnr        = s_matnr-low.
      sl_ret-parceiro     = s_parc-low.
      sl_ret-unitario     = sl_vinc-netwrt / sl_vinc-menge.
      sl_ret-id_export    = sl_export-id_export.

      ADD: sl_vinc-menge  TO vl_qtd   ,
           sl_vinc-netwrt TO vl_netwrt.
      APPEND sl_ret TO tl_ret.
      CLEAR: sl_vinc,
             sl_ret .
    ENDLOOP.
    CLEAR: p_nfenum.

    SELECT SINGLE nfenum INTO p_nfenum
      FROM j_1bnfdoc
      WHERE docnum EQ vl_docnum.

    READ TABLE t_vinc INTO sl_vinc INDEX 1.
    sl_export-docnum       = vl_docnum.
    sl_export-nf_retorno   = p_nfenum.
    sl_export-werks        = p_werks.
    sl_export-data_criacao = sy-datum.
    sl_export-quant        = vl_qtd.
    sl_export-matnr        = s_matnr-low.
    sl_export-valor_total  = vl_netwrt.
    sl_export-export       = space.
    sl_export-status       = 'X'.
    sl_export-finalidade   = p_final.

    LOOP AT it_ret_ter ASSIGNING FIELD-SYMBOL(<fs_ret_ter>).
      <fs_ret_ter>-docnum_ret = vl_docnum.
    ENDLOOP.

    IF NOT tl_ret[] IS INITIAL.
      INSERT zsdt_retlote FROM TABLE tl_ret.
      IF sy-subrc NE 0.
        _error = 'X'.
      ENDIF.
    ENDIF.

    IF NOT sl_export IS INITIAL.
      sl_export-redex = 'X'.
      MODIFY zsdt_export FROM sl_export.
      IF sy-subrc NE 0.
        _error = 'X'.
      ENDIF.
    ENDIF.

*    IF it_ret_ter[] IS NOT INITIAL.
*      INSERT zsdt_retlote_ter FROM TABLE it_ret_ter.
*      IF sy-subrc NE 0.
*        _error = 'X'.
*      ENDIF.
*    ENDIF.

    IF _error IS INITIAL.
      MESSAGE i836 WITH TEXT-016 vl_docnum.
      SET PARAMETER ID 'JEF' FIELD vl_docnum.

      CASE sy-tcode.
        WHEN 'ZSDT0066'.
          docnum = vl_docnum.
          EXPORT: docnum TO MEMORY ID 'ZRET'.
      ENDCASE.

      IF v_tcode EQ 'ZSDT0066'. "81360 LP
        docnum = vl_docnum.
        EXPORT: docnum TO MEMORY ID 'ZRET'.
        id_redex = sl_export-id_export.
        EXPORT: id_redex TO MEMORY ID 'ZRET_REDEX'.
      ENDIF.

      IF ( s_nf_terc IS NOT INITIAL ). "Lançamento Terceiro.
        CALL FUNCTION 'Z_INFO_NFE_FORNECEDOR_GERAL_01'
          EXPORTING
            i_docnum = vl_docnum.
      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      DATA: i_doc    TYPE j_1bnfdoc,
            i_acttab TYPE j_1bnfe_active.

*Realizar do de 4 times para prosseguimento
      DO 04 TIMES.
        SELECT SINGLE * INTO i_doc
                 FROM j_1bnfdoc
                WHERE docnum EQ vl_docnum.
        IF i_doc IS INITIAL.
          WAIT UP TO 2 SECONDS.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

* caso encontrado segue com busca
      SELECT SINGLE * INTO i_acttab
                FROM j_1bnfe_active
               WHERE docnum EQ vl_docnum.

      i_doc-authcod    = lwa_zib_nfe_dist_ter-nr_protocolo.
      i_doc-authdate   = lwa_zib_nfe_dist_ter-dt_protocolo.
      i_doc-authtime   = lwa_zib_nfe_dist_ter-hr_protocolo.

      i_acttab-authcod  = lwa_zib_nfe_dist_ter-nr_protocolo.
      i_acttab-authdate = lwa_zib_nfe_dist_ter-dt_protocolo.
      i_acttab-authtime = lwa_zib_nfe_dist_ter-hr_protocolo.
      i_acttab-action_requ = 'C'.

      CALL FUNCTION 'J_1B_NFE_UPDATE_ACTIVE'
        EXPORTING
          i_acttab  = i_acttab
          i_doc     = i_doc
          i_updmode = 'U'.

      COMMIT WORK.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      MESSAGE e836 WITH TEXT-029 TEXT-031 vl_docnum.
    ENDIF.

    CLEAR: lwa_zib_nfe_dist_ter.
  ENDIF.
ENDFORM.                    " Z_EMITE_NF_REDEX



*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_TAX                                           *
*&---------------------------------------------------------------------*
*                            Preenche Item TAX                         *
*----------------------------------------------------------------------*
FORM z_preenche_tax TABLES t_item_tax STRUCTURE bapi_j_1bnfstx
                     USING p_taxtyp   TYPE c
                           p_itmnum   TYPE j_1bnflin-itmnum.

  DATA sl_item_tax TYPE bapi_j_1bnfstx.

  sl_item_tax-itmnum = p_itmnum.
  sl_item_tax-taxtyp = p_taxtyp.
  sl_item_tax-othbas = s_val-netwrt.

  IF p_redex = 'X'.
    sl_item_tax-othbas = s_val-nftot.
  ENDIF.

  APPEND sl_item_tax TO t_item_tax.

ENDFORM.                    " Z_PREENCHE_TAX

*&---------------------------------------------------------------------*
*&      Form  Z_DETERMINAR_CFOP                                        *
*&---------------------------------------------------------------------*
*                            Determinar CFOP                           *
*---------------------  -------------------------------------------------*
FORM z_determinar_cfop USING p_matnr  TYPE j_1bnflin-matnr
                             p_bukrs  TYPE j_1bnfdoc-bukrs
                             p_branch TYPE j_1bnfdoc-branch
                             p_kunnr  TYPE j_1bnfdoc-parid
                             p_refkey TYPE j_1bnflin-refkey
                    CHANGING p_cfop   TYPE j_1bcfop
                             p_lifnr  TYPE vbpa-lifnr.

  DATA: vl_regio   TYPE kna1-regio,
        vl_region  TYPE adrc-region,
        vl_adrnr   TYPE j_1bbranch-adrnr,
        vl_destino TYPE zsdt_retcfop-destino.

  CLEAR p_cfop.

  SELECT SINGLE lifnr
    FROM vbpa
    INTO p_lifnr
  WHERE  vbeln EQ p_refkey
    AND  parvw EQ c_parvw.

  SELECT SINGLE regio
    FROM lfa1
    INTO vl_regio
  WHERE  lifnr EQ p_lifnr.

  SELECT SINGLE adrnr
    FROM j_1bbranch
    INTO vl_adrnr
  WHERE  bukrs  EQ p_bukrs
    AND  branch EQ p_branch.

  SELECT SINGLE region
    FROM adrc
    INTO vl_region
  WHERE  addrnumber EQ vl_adrnr.

  IF vl_regio NE vl_region.
    vl_destino = space.
  ELSE.
    vl_destino = 'X'.
  ENDIF.

  SELECT SINGLE cfop
    FROM zsdt_retcfop
    INTO p_cfop
  WHERE  matnr   EQ p_matnr
    AND  destino EQ vl_destino.

ENDFORM.                    " Z_DETERMINAR_CFOP

*&---------------------------------------------------------------------*
*&      Form  Z_RET_ERROS                                              *
*&---------------------------------------------------------------------*
*                          Retorna Erros da BAPI                       *
*----------------------------------------------------------------------*
FORM z_ret_erros TABLES t_return STRUCTURE bapiret2.

  DATA: sl_return TYPE bapiret2,
        sl_erro   TYPE type_erro.

  REFRESH t_erro.

  LOOP AT t_return INTO sl_return.

    sl_erro-tipo   = sl_return-type.
    sl_erro-numero = sl_return-number.
    sl_erro-msg    = sl_return-message.

    APPEND sl_erro TO t_erro.

    CLEAR: sl_return,
           sl_erro  .

  ENDLOOP.

  CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
    TABLES
      table    = t_erro
    EXCEPTIONS
      fb_error = 1
      OTHERS   = 2.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " Z_RET_ERROS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VFBA                                         *
*&---------------------------------------------------------------------*
*                          Selecionar VBFA                             *
*----------------------------------------------------------------------*
FORM z_seleciona_vfba.

  DATA: sl_vbrk TYPE type_vbrk,
        sl_vbfa TYPE type_vbfa.

  REFRESH t_vbfa.

  CHECK NOT t_vbrk[] IS INITIAL.

  SELECT vbelv posnv vbeln
         posnn vbtyp_n vbtyp_v
         erdat erzet
    FROM vbfa
    INTO TABLE t_vbfa
    FOR ALL ENTRIES IN t_vbrk
  WHERE  vbelv EQ t_vbrk-vbeln.

  DELETE t_vbfa WHERE: vbtyp_n NE 'M'
                  AND  vbtyp_n NE 'N'.

  SORT t_vbfa BY vbelv   ASCENDING
                 erdat   DESCENDING
                 erzet   DESCENDING.

  LOOP AT t_vbrk INTO sl_vbrk.

    v_index = sy-tabix.

    READ TABLE t_vbfa INTO sl_vbfa
      WITH KEY vbelv = sl_vbrk-vbeln
      BINARY SEARCH.

    IF NOT sy-subrc IS INITIAL.
      DELETE t_vbrk INDEX v_index.
      CONTINUE.
    ENDIF.

    IF sl_vbfa-vbtyp_n EQ 'N'.
      DELETE t_vbrk INDEX v_index.
      CONTINUE.
    ENDIF.

    CLEAR: sl_vbrk,
           sl_vbfa.

  ENDLOOP.

  IF t_vbrk[] IS INITIAL.
    MESSAGE i836 WITH TEXT-009.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_VFBA

*&---------------------------------------------------------------------*
*&      Form  Z_PRENCHE_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_prenche_msg TABLES p_msg     STRUCTURE bapi_j_1bnfftx
                    USING p_linnum  TYPE n
                          p_message TYPE c.

  DATA sl_msg TYPE bapi_j_1bnfftx.

  sl_msg-linnum  = p_linnum.
  sl_msg-message = p_message.

  APPEND sl_msg TO p_msg.

ENDFORM.                    " Z_PRENCHE_MSG

*&---------------------------------------------------------------------*
*&      Form  Z_IMPRIME_NFS                                            *
*&---------------------------------------------------------------------*
*                       Impressão NF's Vinculadas                      *
*----------------------------------------------------------------------*
FORM z_imprime_nfs.

  CALL TRANSACTION 'ZSDT0006'.

ENDFORM.                    " Z_IMPRIME_NFS

*&---------------------------------------------------------------------*
*&      Form  Z_VALIDA_LOTE                                            *
*&---------------------------------------------------------------------*
*                            Válida Lote                               *
*----------------------------------------------------------------------*
FORM z_valida_lote.
  IF s_safra IS INITIAL.
    MESSAGE s836 WITH 'Informe a safra.'.
    SET CURSOR FIELD s_safra.
    ADD 1 TO v_erro.
  ENDIF.

  CHECK NOT s_safra[] IS INITIAL.

  SELECT SINGLE charg
    FROM mch1
    INTO s_safra-low
  WHERE  charg EQ s_safra-low.

  CHECK NOT sy-subrc IS INITIAL.
  MESSAGE e836 WITH TEXT-019.

ENDFORM.                    " Z_VALIDA_LOTE

*&---------------------------------------------------------------------*
*&      Module  SET_PESQ_DCO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_pesq_dco INPUT.
  CLEAR: s_pqsdco.
ENDMODULE.                 " SET_PESQ_DCO  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIKP                                         *
*&---------------------------------------------------------------------*
*                           Seleciona Remessa                          *
*----------------------------------------------------------------------*
FORM z_seleciona_likp.

  REFRESH t_likp.

  SELECT vbeln
    FROM likp AS a
    INTO TABLE t_likp
      WHERE  a~vstel EQ p_werks
        AND  a~vkorg EQ p_bukrs
        AND  a~kunnr IN s_kunnr
        AND  a~fkarv EQ p_fkart
        AND  a~vbeln IN p_vbeln
        AND EXISTS ( SELECT vbeln
                       FROM lips AS b
                      WHERE b~vbeln EQ a~vbeln
                        AND b~matnr IN s_matnr
                        AND b~lgort IN s_lgort
                        AND b~charg IN s_safra ).

  IF ( t_likp[] IS INITIAL ) AND ( t_lin[] IS INITIAL ).
    MESSAGE i836 WITH TEXT-022.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_LIKP

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIPS                                         *
*&---------------------------------------------------------------------*
*                       Seleciona Itens da Remessa                     *
*----------------------------------------------------------------------*
FORM z_seleciona_lips.

  REFRESH t_lips.
  CHECK NOT t_likp[] IS INITIAL.

  SELECT vbeln posnr vgbel vgpos
    FROM lips
    INTO TABLE t_lips
    FOR ALL ENTRIES IN t_likp
  WHERE  vbeln EQ t_likp-vbeln
    AND  matnr IN s_matnr
    AND  lgort IN s_lgort
    AND  charg IN s_safra.

  IF ( t_lips[] IS INITIAL ) AND ( t_lin[] IS INITIAL ).
    MESSAGE i836 WITH TEXT-022.
    LEAVE LIST-PROCESSING.
  ENDIF.

  " DEVK9A229G - SD - Incluir filtro OV. transação ZSDT0008 #143752 RSA
  READ TABLE s_ov INTO DATA(wa_ov) WITH KEY sign = 'I'.
  IF sy-subrc EQ 0.
    DELETE t_lips WHERE vgbel NOT IN s_ov.
  ENDIF.

  READ TABLE s_ov INTO wa_ov WITH KEY sign = 'E'.
  IF sy-subrc EQ 0.
    DELETE t_lips WHERE vgbel NOT IN s_ov.
  ENDIF.
  " DEVK9A229G - SD - Incluir filtro OV. transação ZSDT0008 #143752 RSA

ENDFORM.                    " Z_SELECIONA_LIPS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBPA_2                                       *
*&---------------------------------------------------------------------*
*                           Seleciona VBPA                             *
*----------------------------------------------------------------------*
FORM z_seleciona_vbpa_2.

  TYPES: BEGIN OF ty_lin,
           docnum TYPE j_1bnflin-docnum,
           refkey TYPE j_1bnflin-refkey,
           refitm TYPE j_1bnflin-refitm,
         END OF ty_lin,

         BEGIN OF ty_vbrp,
           vbeln  TYPE vbrp-vbeln,
           posnr  TYPE vbrp-posnr,
           vgbel  TYPE vbrp-vgbel,
           refkey TYPE j_1bnflin-refkey,
         END OF ty_vbrp.

  DATA: tl_vbrp_aux TYPE TABLE OF type_vbrp_aux,
        tl_lin      TYPE TABLE OF ty_lin,
        tl_lin_2    TYPE TABLE OF ty_lin,
        tl_vbrp     TYPE TABLE OF ty_vbrp,
        tl_correc   TYPE TABLE OF type_correc.

  DATA: sl_lips     TYPE lips,
        sl_vbrp_aux TYPE type_vbrp_aux,
        sl_lin      TYPE ty_lin,
        sl_vbrp     TYPE ty_vbrp,
        sl_correc   TYPE type_correc,
        sl_vbpa     TYPE type_vbpa,
        vl_index    TYPE i.

  REFRESH t_vbpa.
  CHECK NOT t_lips[] IS INITIAL.

  SELECT vbeln posnr parvw kunnr lifnr
    FROM vbpa
    INTO CORRESPONDING FIELDS OF TABLE t_vbpa
    FOR ALL ENTRIES IN t_lips
  WHERE  vbeln EQ t_lips-vbeln
    AND  parvw EQ c_parvw.

*** Stefanini - IR196638 - 12/09/2024 - LAZAROSR - Início de Alteração
*  SELECT vbeln posnr vgbel vbeln
*    FROM vbrp
*      INTO TABLE tl_vbrp
*      FOR ALL ENTRIES IN t_lips
*         WHERE vgbel EQ t_lips-vbeln
*           AND vgpos EQ t_lips-posnr AND draft = space.

  SELECT vbrp~vbeln
         vbrp~posnr
         vbrp~vgbel
         vbrp~vbeln
    FROM vbrp
    INNER JOIN vbrk
            ON vbrk~vbeln = vbrp~vbeln
      INTO TABLE tl_vbrp
      FOR ALL ENTRIES IN t_lips
         WHERE vgbel      EQ t_lips-vbeln
           AND vgpos      EQ t_lips-posnr
           AND vbrp~draft EQ space
           AND vbrk~fksto EQ space.
*** Stefanini - IR196638 - 12/09/2024 - LAZAROSR - Fim de Alteração

*  IF TL_VBRP[] IS NOT INITIAL.
*    SELECT VBELV POSNV VBELN
*        POSNN VBTYP_N VBTYP_V
*        ERDAT ERZET
*     FROM VBFA
*     INTO TABLE T_VBFA_N
*     FOR ALL ENTRIES IN TL_VBRP
*     WHERE VBELV   = TL_VBRP-vbeln
*     AND   VBTYP_N = 'N'
*     AND   VBTYP_V = 'M'.
*
*  ENDIF.

  "ALRS
  IF tl_vbrp[] IS NOT INITIAL.
    SELECT j_1bnflin~docnum j_1bnflin~refkey j_1bnflin~refitm
    FROM j_1bnflin
    INNER JOIN j_1bnfe_active
      ON j_1bnfe_active~docnum = j_1bnflin~docnum
      AND   j_1bnfe_active~cancel     = ''
      AND   j_1bnfe_active~docsta     = '1'
    INTO TABLE tl_lin_2
      FOR ALL ENTRIES IN tl_vbrp
        WHERE j_1bnflin~refkey EQ tl_vbrp-refkey
        AND   j_1bnflin~refitm EQ tl_vbrp-posnr.
  ENDIF.


  "ALRS
  SORT tl_lin_2 BY refkey.
  LOOP AT tl_vbrp INTO sl_vbrp.
    READ TABLE tl_lin_2 INTO sl_lin WITH KEY refkey = sl_vbrp-refkey.
    IF sy-subrc EQ 0.
      sl_vbrp_aux-refkey = sl_vbrp-vbeln.
      sl_vbrp_aux-posnr  = sl_vbrp-posnr.
      sl_vbrp_aux-vgbel  = sl_vbrp-vgbel.
      APPEND sl_vbrp_aux TO tl_vbrp_aux.
      CLEAR: sl_vbrp_aux.
    ENDIF.
  ENDLOOP.

*  LOOP AT TL_VBRP INTO SL_VBRP.
*    READ TABLE T_VBFA_N WITH KEY VBELN = SL_VBRP-VBELN.
*    IF SY-SUBRC NE 0.
*      READ TABLE T_VBFA_N WITH KEY VBELV = SL_VBRP-VBELN.
*      IF SY-SUBRC NE 0.
*        SL_VBRP_AUX-REFKEY = SL_VBRP-VBELN.
*        SL_VBRP_AUX-POSNR  = SL_VBRP-POSNR.
*        SL_VBRP_AUX-VGBEL  = SL_VBRP-VGBEL.
*        APPEND SL_VBRP_AUX TO TL_VBRP_AUX.
*        CLEAR: SL_VBRP_AUX.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

  SELECT docnum refkey refitm
    FROM j_1bnflin
    INTO TABLE tl_lin
      FOR ALL ENTRIES IN tl_vbrp_aux
        WHERE refkey EQ tl_vbrp_aux-refkey
          AND refitm EQ tl_vbrp_aux-posnr.

  SELECT docnum novo_terminal id_cc
    FROM zcarta_correcao
      INTO TABLE tl_correc
      FOR ALL ENTRIES IN tl_lin
        WHERE docnum EQ tl_lin-docnum
       AND   novo_terminal NE ''.
  SORT  tl_correc BY docnum ASCENDING id_cc DESCENDING.

  SORT: tl_vbrp_aux BY vgbel.
  IF sy-subrc IS INITIAL.
    LOOP AT t_vbpa INTO sl_vbpa.
      READ TABLE tl_vbrp_aux INTO sl_vbrp_aux
        WITH KEY vgbel = sl_vbpa-vbeln
                 BINARY SEARCH.

      READ TABLE tl_lin INTO sl_lin
       WITH KEY refkey = sl_vbrp_aux-refkey
                refitm = sl_vbrp_aux-posnr.

      READ TABLE tl_correc INTO sl_correc
         WITH KEY docnum = sl_lin-docnum.
      IF sy-subrc IS  INITIAL.
        IF sl_correc-novo_terminal IS NOT INITIAL.
          sl_vbpa-lifnr =  sl_correc-novo_terminal.
          MODIFY t_vbpa FROM sl_vbpa.
        ENDIF.
      ELSE.
        IF sl_vbpa-lifnr NOT IN s_parc[].
          DELETE t_vbpa.
        ENDIF.
      ENDIF.
    ENDLOOP.
    DELETE t_vbpa WHERE lifnr NOT IN s_parc[].
  ELSE.
    DELETE t_vbpa WHERE lifnr NOT IN s_parc[].
  ENDIF.

  LOOP AT t_vbpa INTO sl_vbpa.
    READ TABLE tl_vbrp_aux INTO sl_vbrp_aux
       WITH KEY vgbel = sl_vbpa-vbeln
                BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      MOVE sl_vbrp_aux-refkey TO sl_vbpa-refkey.

      MODIFY t_vbpa FROM sl_vbpa.
    ENDIF.
  ENDLOOP.

  SORT t_vbpa BY vbeln ASCENDING
                 posnr ASCENDING.

  IF ( t_vbpa[] IS INITIAL )  AND ( t_lin[] IS INITIAL ).
    MESSAGE i836 WITH TEXT-022.
    LEAVE LIST-PROCESSING.
    EXIT.
  ENDIF.

  IF t_vbpa[] IS INITIAL.
    CLEAR: t_lips[], t_likp[].
  ENDIF.
**********
  CHECK NOT t_vbpa[] IS INITIAL.

  LOOP AT t_lips ASSIGNING FIELD-SYMBOL(<lips>).
*    VL_INDEX = SY-TABIX.
    IF NOT line_exists( t_vbpa[ vbeln = <lips>-vbeln ] ).
      <lips>-vbeln = 'W'.
    ENDIF.

*    READ TABLE T_VBPA
*      WITH KEY VBELN = SL_LIPS-VBELN
*      BINARY SEARCH
*      TRANSPORTING NO FIELDS.
*    IF NOT SY-SUBRC IS INITIAL.
*      DELETE T_LIPS INDEX VL_INDEX.
*    ENDIF.
*    CLEAR SL_LIPS.
  ENDLOOP.

  DELETE t_lips WHERE vbeln EQ 'W'.

  IF ( t_lips[] IS INITIAL ) AND ( t_lin[] IS INITIAL ).
    MESSAGE i836 WITH TEXT-022.
    LEAVE LIST-PROCESSING.
  ENDIF.
***

ENDFORM.                    " Z_SELECIONA_VBPA_2

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBFA                                         *
*&---------------------------------------------------------------------*
*                              Selecionar VBFA                         *
*----------------------------------------------------------------------*
FORM z_seleciona_vbfa.

  TYPES: BEGIN OF ty_vbfa_aux,
           vgbel TYPE vgbel,
           vgpos TYPE vgpos,
         END OF ty_vbfa_aux.

  DATA: vl_vbeln    TYPE vbak-vbeln,
        sl_vbfa     TYPE type_vbfa,
        sl_vbpa     TYPE type_vbpa,
        it_vbfa_aux TYPE TABLE OF ty_vbfa_aux.

  DATA: r_vbeln TYPE RANGE OF vbeln. "ZSDT0008 - Adicionar filtro instrucao - BG #128346 INICIO

  CHECK NOT t_lips[] IS INITIAL.

  it_vbfa_aux[] = VALUE #( FOR _lips IN t_lips[] (
    vgbel = _lips-vgbel
    vgpos = _lips-vgpos
  ) ).
  SORT it_vbfa_aux[] BY vgbel vgpos ASCENDING.
  DELETE ADJACENT DUPLICATES FROM it_vbfa_aux[] COMPARING vgbel vgpos.
  "ZSDT0008 - Adicionar filtro instrucao - BG #128346 INICIO
  IF s_inst-low IS NOT INITIAL.

    SELECT 'EQ' AS option, 'I' AS sign, vbeln AS low
    INTO CORRESPONDING FIELDS OF TABLE @r_vbeln
    FROM zsdt0066
    WHERE instrucao EQ @s_inst-low AND vbeln IS NOT NULL.

    DELETE it_vbfa_aux[] WHERE vgbel NOT IN r_vbeln.

  ENDIF.
  CHECK NOT it_vbfa_aux[] IS INITIAL.
  "ZSDT0008 - Adicionar filtro instrucao - BG #128346 - FIM
  SELECT vbelv posnv vbeln
         posnn vbtyp_n vbtyp_v
         erdat erzet
    FROM vbfa
    INTO TABLE t_vbfa
    FOR ALL ENTRIES IN it_vbfa_aux[]
  WHERE  vbelv   EQ it_vbfa_aux-vgbel
    AND  posnv   EQ it_vbfa_aux-vgpos
    AND  vbtyp_n IN ('H', 'M' ).

*  SELECT VBELV POSNV VBELN
*         POSNN VBTYP_N VBTYP_V
*         ERDAT ERZET
*    FROM VBFA
*    INTO TABLE T_VBFA
*    FOR ALL ENTRIES IN T_LIPS
*  WHERE  VBELV   EQ T_LIPS-VGBEL
*    AND  POSNV   EQ T_LIPS-VGPOS
*    AND  VBTYP_N IN ('H', 'M' ).

*  DELETE T_VBFA WHERE VBTYP_N NE 'H' AND VBTYP_N NE 'M'.

  CLEAR: t_vbfa_3[].

  MOVE t_vbfa[] TO t_vbfa_3[].
  DELETE t_vbfa   WHERE vbtyp_n NE 'H'.
  DELETE t_vbfa_3 WHERE vbtyp_n NE 'M'.

  IF ( t_vbfa[] IS INITIAL ) AND ( t_vbfa_3[] IS INITIAL ) AND (  t_lin[] IS INITIAL ).
    MESSAGE i836 WITH TEXT-023.
    LEAVE LIST-PROCESSING.
    EXIT.
  ENDIF.

  IF ( t_vbfa[] IS INITIAL ) AND ( t_vbfa_3[] IS INITIAL ).
    CHECK 1 = 2.
  ENDIF.

  IF NOT s_dco IS INITIAL.
    SELECT SINGLE vbeln
      FROM zdco_produtor
      INTO vl_vbeln
    WHERE nr_dco EQ s_dco.
    DELETE t_vbfa WHERE vbelv NE vl_vbeln.
    IF t_vbfa[] IS INITIAL.
      MESSAGE i836 WITH TEXT-021.
      LEAVE LIST-PROCESSING.
      EXIT.
    ENDIF.
  ENDIF.

  IF NOT t_vbfa[] IS INITIAL.
    SELECT vbelv posnv vbeln
           posnn vbtyp_n vbtyp_v
           erdat erzet
      FROM vbfa
      INTO TABLE t_vbfa_2
      FOR ALL ENTRIES IN t_vbfa
    WHERE  vbeln   EQ t_vbfa-vbeln
      AND  posnn   EQ t_vbfa-posnn
      AND  vbtyp_n EQ 'H'
      AND  vbtyp_v EQ 'M'.
  ENDIF.

  IF NOT t_vbfa_3[] IS INITIAL.
    LOOP AT t_vbfa_3 INTO sl_vbfa.
      READ TABLE t_vbpa INTO sl_vbpa
        WITH KEY refkey = sl_vbfa-vbeln.
      IF sy-subrc IS INITIAL.
        sl_vbfa-vbelv = sl_vbfa-vbeln.
        APPEND sl_vbfa TO t_vbfa_2.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF ( t_vbfa_2[] IS INITIAL )  AND (  t_lin[] IS INITIAL ).
    MESSAGE i836 WITH TEXT-009.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_VBFA

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBRK_2                                       *
*&---------------------------------------------------------------------*
*                             Seleciona VBRK                           *
*----------------------------------------------------------------------*
FORM z_seleciona_vbrk_2.

  DATA sl_vbrk TYPE type_vbrk.

  REFRESH t_vbrk.
  CHECK NOT t_vbfa_2[] IS INITIAL.

  SELECT vbeln fkart kunrg xblnr
    FROM vbrk
    INTO TABLE t_vbrk
    FOR ALL ENTRIES IN t_vbfa_2
  WHERE  vbeln EQ t_vbfa_2-vbelv.

  SORT t_vbrk BY vbeln ASCENDING.

  IF ( t_vbrk[] IS INITIAL ) AND ( t_lin[] IS INITIAL ).
    MESSAGE i836 WITH TEXT-009.
    LEAVE LIST-PROCESSING.
    EXIT.
  ENDIF.

  LOOP AT t_vbrk INTO sl_vbrk.
    v_index = sy-tabix.
    sl_vbrk-refkey = sl_vbrk-vbeln.
    MODIFY t_vbrk FROM sl_vbrk
      INDEX v_index
      TRANSPORTING refkey.
    CLEAR sl_vbrk.
  ENDLOOP.

ENDFORM.                    " Z_SELECIONA_VBRK_2
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LIN_ZW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_lin_zw .

  DATA: it_zfiwrt0015 TYPE TABLE OF zfiwrt0015 WITH HEADER LINE,
        it_j_1bnfdoc  TYPE TABLE OF j_1bnfdoc  WITH HEADER LINE,
        t_lin_aux     TYPE TABLE OF type_lin   WITH HEADER LINE,
        r_centro      TYPE RANGE OF werks_d,
        tl_correc     TYPE TABLE OF zcarta_correcao WITH HEADER LINE.

  REFRESH: t_lin, tl_correc, it_j_1bnfdoc[].
*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255
*  SELECT *
*    FROM zsdt_depara_depo
*    INTO TABLE @DATA(it_depara_depo)
*    WHERE werks EQ @p_werks
*      AND lifnr IN @s_parc.

  zcl_depara_centro_fixo_virtual=>get_dados_depara(
    EXPORTING
      i_werks        = p_werks
      i_lifnr        = CONV #( s_parc-low )
    IMPORTING
      e_table_depara = DATA(it_depara_depo) ).

*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255  - FIM

  FREE r_centro.

  APPEND VALUE #( option = 'EQ' sign = 'I' low = p_werks ) TO r_centro.

  LOOP AT it_depara_depo INTO DATA(wa_depara_depo).
    APPEND VALUE #( option = 'EQ' sign = 'I' low = wa_depara_depo-werks_v ) TO r_centro.
  ENDLOOP.

  SELECT l~docnum l~itmnum l~matnr l~maktx
         l~bwkey  l~matkl  l~nbm
         l~taxsit l~taxsi2 l~matuse
         l~refkey l~menge  l~meins
         l~netpr  l~netwr  l~netwrt
         l~taxlw1 l~taxlw2 l~itmtyp
         l~werks  l~taxlw3 l~taxlw4
         z~seq_lcto l~reftyp z~ctrl_zrfl
    FROM j_1bnflin AS l
   INNER JOIN  zfiwrt0008 AS z ON z~docnum EQ l~docnum "AND z~ctrl_zrfl EQ 'S'
    INTO TABLE t_lin
   WHERE reftyp EQ 'ZW'
     AND charg  IN s_safra
*     AND werks  EQ p_werks
     AND werks  IN r_centro
     AND matnr  IN s_matnr.

  DELETE t_lin WHERE ctrl_zrfl <> 'S'.

  DELETE t_lin WHERE docnum NOT IN s_docnum. "// wbarbosa 31102024 - US-153330

  "AND NOT EXISTS ( SELECT * FROM ZLEST0172 AS N WHERE N~DOCNUM EQ L~DOCNUM ).

  DELETE t_lin WHERE docnum IS INITIAL.
  CHECK t_lin[] IS NOT INITIAL.

  SELECT *
    FROM j_1bnfdoc INTO TABLE it_j_1bnfdoc
     FOR ALL ENTRIES IN t_lin
   WHERE docnum = t_lin-docnum.

*  DELETE it_j_1bnfdoc WHERE branch NE p_werks.
  IF r_centro IS NOT INITIAL.
    DELETE it_j_1bnfdoc WHERE branch NOT IN r_centro.
  ENDIF.

  LOOP AT t_lin.
    READ TABLE it_j_1bnfdoc WITH KEY docnum = t_lin-docnum.
    IF sy-subrc NE 0.
      DELETE t_lin.
    ENDIF.
  ENDLOOP.

  CHECK t_lin[] IS NOT INITIAL.

  SELECT *
      FROM zcarta_correcao
        INTO TABLE tl_correc
        FOR ALL ENTRIES IN t_lin
          WHERE docnum EQ t_lin-docnum
          AND   novo_terminal NE ''.
  SORT  tl_correc BY docnum ASCENDING id_cc DESCENDING.

  IF NOT s_parc IS INITIAL.
*    DELETE TL_CORREC WHERE NOVO_TERMINAL NOT IN S_PARC.
    CLEAR: t_lin_aux[].
    MOVE t_lin[] TO t_lin_aux[].
    SORT t_lin_aux BY seq_lcto.
    DELETE ADJACENT DUPLICATES FROM t_lin_aux COMPARING seq_lcto.

    SELECT *
      INTO TABLE it_zfiwrt0015
      FROM zfiwrt0015
       FOR ALL ENTRIES IN t_lin_aux
     WHERE seq_lcto EQ t_lin_aux-seq_lcto
*       AND PARID    IN S_PARC
       AND parvw    EQ 'Z1'.

    LOOP AT it_zfiwrt0015.
      READ TABLE t_lin_aux WITH KEY seq_lcto = it_zfiwrt0015-seq_lcto.
      IF sy-subrc IS INITIAL.
        READ TABLE tl_correc
        WITH KEY docnum  = t_lin_aux-docnum.
        IF sy-subrc IS INITIAL.
          IF tl_correc-novo_terminal IS NOT INITIAL.
            it_zfiwrt0015-parid = tl_correc-novo_terminal.
            MODIFY it_zfiwrt0015.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    DELETE it_zfiwrt0015 WHERE parid NOT IN s_parc.

    LOOP AT t_lin_aux.
      READ TABLE it_zfiwrt0015 WITH KEY seq_lcto = t_lin_aux-seq_lcto.
      IF sy-subrc IS NOT INITIAL.
        DELETE t_lin WHERE seq_lcto = t_lin_aux-seq_lcto.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " Z_SELECIONA_LIN_ZW

FORM z_seleciona_lin_rem_dep_fec.

  RANGES: r_lifnr FOR lfa1-lifnr.


  CLEAR: r_lifnr[].

  CHECK s_kunnr[] IS NOT INITIAL.

  SELECT SINGLE *
    FROM kna1 INTO @DATA(_wl_kna1)
   WHERE kunnr IN @s_kunnr.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE *
    FROM lfa1 INTO @DATA(_wl_lfa1)
   WHERE lifnr IN @s_parc.

  CHECK sy-subrc EQ 0.
  "Clinte           Terminal
  CHECK ( p_depfec IS NOT INITIAL ) AND ( _wl_kna1-stcd1 EQ _wl_lfa1-stcd1 ).

  SELECT l~docnum l~itmnum l~matnr l~maktx
         l~bwkey  l~matkl  l~nbm
         l~taxsit l~taxsi2 l~matuse
         l~refkey l~menge  l~meins
         l~netpr  l~netwr  l~netwrt
         l~taxlw1 l~taxlw2 l~itmtyp
         l~werks  l~taxlw3 l~taxlw4 l~reftyp
    FROM j_1bnflin AS l APPENDING CORRESPONDING FIELDS OF TABLE t_lin
   WHERE l~reftyp IN ( 'MD' , 'BI' )
     AND l~charg  IN s_safra
     AND l~werks  EQ p_werks
     AND l~matnr  IN s_matnr
     AND l~docnum IN s_docnum "// wbarbosa 31102024 - US-153330
     AND l~cfop   EQ '5905AA'
     AND EXISTS ( SELECT *
                    FROM j_1bnfdoc AS a
                   WHERE a~docnum EQ l~docnum
                     AND a~parid  IN s_kunnr
                     AND a~partyp EQ 'C' ).

  SELECT l~docnum l~itmnum l~matnr l~maktx
         l~bwkey  l~matkl  l~nbm
         l~taxsit l~taxsi2 l~matuse
         l~refkey l~menge  l~meins
         l~netpr  l~netwr  l~netwrt
         l~taxlw1 l~taxlw2 l~itmtyp
         l~werks  l~taxlw3 l~taxlw4 l~reftyp
    FROM j_1bnflin AS l APPENDING CORRESPONDING FIELDS OF TABLE t_lin
   WHERE l~reftyp IN ( 'MD' , 'BI' )
     AND l~charg  IN s_safra
     AND l~werks  EQ p_werks
     AND l~matnr  IN s_matnr
     AND l~docnum IN s_docnum "// wbarbosa 31102024 - US-153330
     AND l~cfop   EQ '5905AA'
     AND EXISTS ( SELECT *
                    FROM j_1bnfdoc AS a
                   WHERE a~docnum EQ l~docnum
                     AND a~parid  IN s_parc
                     AND a~partyp EQ 'V' ).

  LOOP AT t_lin WHERE reftyp EQ 'MD'.
    DATA(_error) = ''.

    IF strlen( t_lin-refkey ) NE 14 .
      _error = 'X'.
    ENDIF.

    SELECT SINGLE *
      FROM mseg INTO @DATA(_wl_mseg)
     WHERE mblnr EQ @t_lin-refkey+00(10)
       AND mjahr EQ @t_lin-refkey+10(4)
       AND lgort IN @s_lgort.

    IF sy-subrc NE 0.
      _error = 'X'.
    ENDIF.

    IF _error IS NOT INITIAL.
      DELETE t_lin.
    ENDIF.
  ENDLOOP.


ENDFORM.

"WBARBOSA 23102024 - US-153330 --->>>
FORM f_filter_eudr_documents TABLES p_documentos STRUCTURE t_lin.

  DATA: r_docnum TYPE rsis_t_range. "// WBARBOSA 31102024 US-153330

  CHECK p_documentos IS NOT INITIAL.

  r_docnum = VALUE #(
       FOR ws_lin IN p_documentos (
           sign   = zcl_les_utils=>if_stab_constants~mc_sign_include
           option = zcl_les_utils=>if_stab_constants~mc_option_equal
           low    = ws_lin-docnum ) ).

  zcl_eudr_utils=>check_doc_fiscal_eudr(
    EXPORTING
      i_docnum_t    =  r_docnum
    IMPORTING
      e_docnum_eudr =  DATA(lit_docnum_eudr) ).

  DELETE lit_docnum_eudr WHERE eudr NOT IN r_eudr.

  FREE r_docnum.
  r_docnum =  VALUE #( FOR ws_docnum IN lit_docnum_eudr (
                         sign = 'I'
                         option = 'EQ'
                         low    = ws_docnum-docnum ) ).

  DELETE p_documentos WHERE docnum NOT IN r_docnum.

ENDFORM.


"WBARBOSA 23102024 - US-153330 <<<---


*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA_LIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_processa_lin .

  DATA: sl_lin   TYPE type_lin,
        vl_menge TYPE ekpo-menge.

  "// WBARBOSA 31102024 US-153330
  DELETE t_lin WHERE docnum NOT IN s_docnum.

  IF r_eudr IS NOT INITIAL.
    PERFORM f_filter_eudr_documents TABLES t_lin.
  ENDIF.
  "// WBARBOSA 31102024 US-153330

  "SD - Filtra Documentos 1 x 1 IR238009 -  WPP - Ini
  IF sy-tcode EQ 'ZSDT0008' AND it_rfl_zsdt0008[] IS INITIAL AND p_final EQ 'E'.
    PERFORM f_descarta_docs_vinc_1x1 TABLES t_lin.
  ENDIF.
  "SD - Filtra Documentos 1 x 1 IR238009 -  WPP - Fim

  DELETE t_lin WHERE matnr NOT IN s_matnr[].

  SORT t_lin BY docnum ASCENDING
                itmnum ASCENDING.

  IF t_lin[] IS INITIAL.
    MESSAGE i836 WITH TEXT-012.
    LEAVE LIST-PROCESSING.
  ENDIF.

  LOOP AT t_lin INTO sl_lin.

    v_index = sy-tabix.

    "DU-e 26.06.2018 - Ini
    IF it_rfl_zsdt0008[] IS NOT INITIAL.
      READ TABLE it_rfl_zsdt0008 WITH KEY docnum_vinc = sl_lin-docnum.
      IF ( sy-subrc NE 0 ).
        sl_lin-del = abap_true.
        MODIFY t_lin FROM sl_lin INDEX v_index.
        CONTINUE.
      ENDIF.
    ENDIF.
    "DU-e 26.06.2018 - Fim

    CHECK sl_lin-meins EQ 'TO'.

    CALL FUNCTION 'ME_CONVERSION_MEINS'
      EXPORTING
        i_matnr             = sl_lin-matnr
        i_mein1             = sl_lin-meins
        i_meins             = 'KG'
        i_menge             = sl_lin-menge
      IMPORTING
        menge               = vl_menge
      EXCEPTIONS
        error_in_conversion = 1
        no_success          = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      CONTINUE.
    ENDIF.

    sl_lin-meins = 'KG'.
    sl_lin-menge = vl_menge.
    MODIFY t_lin FROM sl_lin
      INDEX v_index
      TRANSPORTING menge meins.
    CLEAR: vl_menge ,
           sl_lin   .

  ENDLOOP.

  DELETE t_lin WHERE del = abap_true.

  IF s_nbm-low IS NOT INITIAL.
    DELETE t_lin WHERE nbm NOT IN s_nbm.
  ENDIF.

  PERFORM z_seleciona_ret   ."   Seleciona ZSDT_RETLOTE

  PERFORM z_check_saldo_docs.

  "Verifica Registro CCT
  PERFORM z_registro_cct.

  IF t_lin[] IS INITIAL.
    MESSAGE i836 WITH TEXT-012.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_PROCESSA_LIN
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR 'TB0200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  DATA: wl_retlote   TYPE zsdt_retlote,
        tl_export    TYPE TABLE OF zsdt_export,
        wl_export    TYPE zsdt_export,
        vl_cont      TYPE i,
        lv_cont_memo TYPE i.

  CLEAR: wl_retlote,
         tl_export,
         wl_export,
         vl_cont,
         lv_cont_memo.


  CASE sy-ucomm.
    WHEN 'CONF' OR 'ENTER'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wg_0200-docnum_ret
        IMPORTING
          output = wg_0200-docnum_ret.

      CHECK wg_0200-docnum_ret IS NOT INITIAL.

      SELECT COUNT(*)
             FROM zsdt_export
            WHERE docnum = wg_0200-docnum_ret.
      IF sy-dbcnt > 1.
        IF wg_0200-id_redex IS INITIAL.
          MESSAGE 'Para processo Redex informar a linha redex' TYPE 'E'.
          EXIT.
        ENDIF.
      ENDIF.

      SELECT SINGLE *
        FROM j_1bnfdoc INTO @DATA(_wl_doc_est)
       WHERE docnum = @wg_0200-docnum_ret.

      IF ( sy-subrc NE 0 ).

        SELECT COUNT(*)
          FROM znom_remetente
          WHERE docnum_rt = wg_0200-docnum_ret.
        IF sy-subrc IS INITIAL.
          UPDATE zsdt0053 SET docnum_rt = ' ' id_export = ' ' WHERE docnum_rt = wg_0200-docnum_ret.
        ENDIF.

        SELECT COUNT(*)
        FROM zsdt_retlote
        WHERE docnum_ret = wg_0200-docnum_ret.
        IF sy-subrc IS INITIAL.
          DELETE FROM zsdt_retlote WHERE docnum_ret = wg_0200-docnum_ret AND id_export = wg_0200-id_redex.
        ENDIF.

        SELECT COUNT(*)
          FROM zsdt_export
         WHERE docnum = wg_0200-docnum_ret
          AND id_export = wg_0200-id_redex.
        IF sy-subrc IS INITIAL.
          DELETE FROM zsdt_export WHERE docnum = wg_0200-docnum_ret AND id_export = wg_0200-id_redex.
        ENDIF.

        SELECT COUNT(*)
           FROM zsdt_export
           WHERE docnum = wg_0200-docnum_ret.
        IF sy-subrc NE 0.
          UPDATE zsdt0053 SET docnum_rt = '0000000000' WHERE docnum_rt = wg_0200-docnum_ret AND id_export = wg_0200-id_redex.
        ENDIF.


*        MESSAGE |Documento { WG_0200-DOCNUM_RET } não encontrado!| TYPE 'W'.
        EXIT.
      ENDIF.

      "10.10.2018 - Retorno não pode mais ser cancelado CS2018002826
*      IF ( _WL_DOC_EST-CANDAT IS INITIAL ). "Data de Estorno
*        MESSAGE |Documento { WG_0200-DOCNUM_RET } ainda encontra-se ativo!| TYPE 'W'.
*        EXIT.
*      ENDIF.

      SELECT SINGLE docnum_ret
        FROM zsdt_retlote INTO CORRESPONDING FIELDS OF wl_retlote
       WHERE docnum_ret = wg_0200-docnum_ret
         AND id_export = wg_0200-id_redex..

      SELECT *
        FROM zsdt_export INTO CORRESPONDING FIELDS OF TABLE tl_export
       WHERE docnum = wg_0200-docnum_ret
        AND id_export = wg_0200-id_redex.                   "LP 81360

      LOOP AT tl_export INTO wl_export.
        IF wl_export-ordem  IS NOT INITIAL OR
           wl_export-export IS NOT INITIAL.
          ADD 1 TO vl_cont.
        ENDIF.
      ENDLOOP.

      IF vl_cont IS INITIAL.
        LOOP AT tl_export INTO wl_export WHERE ordem  IS INITIAL AND
                                               export IS INITIAL.
          DELETE zsdt_export FROM wl_export.
        ENDLOOP.
        DELETE FROM zsdt_retlote WHERE docnum_ret = wg_0200-docnum_ret AND id_export = wg_0200-id_redex.

** Marcos Faneli -> Remove vinculos com memorandos - 11.12.2014 - Ch.130025
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = sy-index
            text       = 'Buscando vinculos com ZMEMO00 ...'.

        SELECT COUNT( DISTINCT docnum_rt )
          FROM znom_remetente
          INTO lv_cont_memo
          WHERE docnum_rt = wg_0200-docnum_ret.

        IF lv_cont_memo IS NOT INITIAL.
          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              percentage = sy-index
              text       = 'Removendo vinculos com ZMEMO00 ...'.

          UPDATE znom_remetente SET docnum_rt = '' WHERE docnum_rt = wg_0200-docnum_ret.
        ENDIF.

        SELECT SINGLE *
          FROM zsdt0053 INTO @DATA(_wl_0053)
         WHERE docnum_rt EQ @wg_0200-docnum_ret
          AND id_export = @wg_0200-id_redex.

        IF sy-subrc EQ 0.
          UPDATE zsdt0053 SET docnum_rt = '0000000000' WHERE docnum_rt = wg_0200-docnum_ret AND id_export = wg_0200-id_redex.
        ENDIF.
        "<<84592 LP - inicio
        MESSAGE TEXT-026 TYPE 'I'.

        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE TEXT-025 TYPE 'I'.
      ENDIF.
*      ELSE.
*        MESSAGE TEXT-024 TYPE 'I'.
*      ENDIF.
      "ELSE.
      "  MESSAGE 'Nenhum documento encontado com o número informado.' TYPE 'I'.
      "ENDIF.
    WHEN 'FECH' OR 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DCO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_dco.
***    Ch.129231 - Marcos Faneli
  DATA: wl_vin_dco TYPE ty_vin_dco,
        lv_ref     TYPE j_1bnflin-refkey.

  REFRESH: t_dco.

  LOOP AT s_dco_.
    TRANSLATE s_dco_-low USING '. '.
    TRANSLATE s_dco_-low USING '- '.
    CONDENSE s_dco_-low NO-GAPS.

    TRANSLATE s_dco_-high USING '. '.
    TRANSLATE s_dco_-high USING '- '.
    CONDENSE s_dco_-high NO-GAPS.

    MODIFY s_dco_.
  ENDLOOP.

*  Encontrando vinculos de DCO.
  SELECT DISTINCT docnum
    FROM zdco_vinculo
    JOIN vbfa ON vbfa~vbelv = zdco_vinculo~vbeln AND
                      vbtyp_n = 'M' AND
                      vbtyp_v = 'J'
    JOIN j_1bnflin ON refkey = vbfa~vbeln
    INTO TABLE t_dco
    WHERE nr_dco IN s_dco_.
ENDFORM.                    " Z_SELECIONA_DCO
*&---------------------------------------------------------------------*
*&      Form  CHECK_CHEGADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_chegada .

  CHECK p_comcct EQ abap_false.

  CHECK NOT t_avinc IS INITIAL.

  SELECT * FROM zlest0039
    INTO TABLE @DATA(zlest0039)
      FOR ALL ENTRIES IN @t_avinc
    WHERE docnum EQ @t_avinc-docnum.

  DATA(_zlest0039) = zlest0039[].

  IF NOT zlest0039 IS INITIAL.

    SELECT * FROM zlest0019
      INTO TABLE @DATA(zlest0019)
        FOR ALL ENTRIES IN @zlest0039
      WHERE bukrs   EQ @zlest0039-bukrs
        AND branch  EQ @zlest0039-werks
        AND nfenum  EQ @zlest0039-nfenum
        AND idinter EQ 'L3'
        AND tp_movi EQ 'E'
        AND tp_reg  EQ '30'.

  ENDIF.

  LOOP AT _zlest0039 ASSIGNING FIELD-SYMBOL(<w0039>)
    WHERE datachegada EQ '00000000'
       OR datachegada EQ ''.

    TRY .
        <w0039>-datachegada = zlest0019[ bukrs   = <w0039>-bukrs
                                         branch  = <w0039>-werks
                                         nfenum  = <w0039>-nfenum
                                       ]-dtachegada.
      CATCH cx_sy_itab_line_not_found.
        <w0039>-datachegada = '00000000'.
    ENDTRY.

    ASSIGN zlest0039[ docnum = <w0039>-docnum ] TO FIELD-SYMBOL(<w39>).
    IF NOT <w39> IS INITIAL.
      <w39>-datachegada = <w0039>-datachegada.
    ENDIF.

  ENDLOOP.

  IF zlest0039[] IS NOT INITIAL.
    SELECT * FROM zlest0060
      INTO TABLE @DATA(zlest0060)
        FOR ALL ENTRIES IN @zlest0039
      WHERE doc_rem EQ @zlest0039-vbeln
        AND chave_nfe EQ @zlest0039-chave_nfe.
  ENDIF.

  IF zlest0060[] IS NOT INITIAL.

    SELECT * FROM zlest0061
        INTO TABLE @DATA(zlest0061)
          FOR ALL ENTRIES IN @zlest0060
        WHERE bukrs EQ @zlest0060-bukrs
          AND werks EQ @zlest0060-werks
          AND ano_viagem EQ @zlest0060-ano_viagem
          AND nr_viagem  EQ @zlest0060-nr_viagem
          AND embarcacao EQ @zlest0060-embarcacao
          AND nome_emb   EQ @zlest0060-nome_emb
          AND dt_movimento EQ @zlest0060-dt_movimento
          AND cl_codigo EQ @zlest0060-cl_codigo
          AND rm_codigo EQ @zlest0060-rm_codigo
          AND safra  EQ @zlest0060-safra
          AND docnum EQ @zlest0060-docnum
          AND id_frete_aqua EQ @zlest0060-id_frete_aqua.
  ENDIF.

  LOOP AT _zlest0039 ASSIGNING <w0039>
    WHERE datachegada EQ '00000000'
       OR datachegada EQ ''.

    TRY .
        DATA(w0060) = zlest0060[ doc_rem   = <w0039>-vbeln
                                 chave_nfe = <w0039>-chave_nfe ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    IF NOT w0060 IS INITIAL.
      TRY .
          <w0039>-datachegada = zlest0061[ bukrs = w0060-bukrs
                                           werks = w0060-werks
                                           ano_viagem = w0060-ano_viagem
                                           nr_viagem = w0060-nr_viagem
                                           embarcacao = w0060-embarcacao
                                           nome_emb = w0060-nome_emb
                                           dt_movimento = w0060-dt_movimento
                                           cl_codigo = w0060-cl_codigo
                                           rm_codigo = w0060-rm_codigo
                                           safra = w0060-safra
                                           docnum = w0060-docnum
                                           id_frete_aqua = w0060-id_frete_aqua
                                         ]-dt_chegada.

        CATCH cx_sy_itab_line_not_found.
          <w0039>-datachegada = '00000000'.
      ENDTRY.
    ENDIF.
  ENDLOOP.

  LOOP AT t_avinc ASSIGNING FIELD-SYMBOL(<f_avinc>).
    TRY .
        <f_avinc>-dtachegada = _zlest0039[ docnum = <f_avinc>-docnum ]-datachegada.
      CATCH cx_sy_itab_line_not_found.
        <f_avinc>-dtachegada = '00000000'.
    ENDTRY.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  Z_SCREEEN_AVINC  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_screeen_avinc OUTPUT.

  LOOP AT SCREEN.
    LOOP AT tc_avinc-cols ASSIGNING FIELD-SYMBOL(<avinc>).
      IF  <avinc>-screen-name EQ screen-name.
        IF NOT s_avinc-dtachegada IS INITIAL.
          <avinc>-screen-intensified = itensidade.
          <avinc>-screen-color = cor.
        ELSE.
          <avinc>-screen-intensified = itensidade - 1.
          <avinc>-screen-color = cor + 5.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.
  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0300 OUTPUT.

ENDMODULE.

FORM z_valida_nf_terc.

  DATA: v_xblnr  TYPE xblnr1,
        v_lifnr  TYPE lifnr,
        v_nftype TYPE j_1bnftype.

  IF p_redex IS INITIAL.

    IF ( p_nfenum IS INITIAL ).
      MESSAGE s836 WITH 'Informe o número da NF-e'.
      ADD 1 TO v_erro.
      EXIT.
    ENDIF.

    IF ( p_series IS INITIAL ).
      MESSAGE s836 WITH 'Informe o número da Série'.
      ADD 1 TO v_erro.
      EXIT.
    ENDIF.

    IF ( p_docdat IS INITIAL ).
      MESSAGE s836 WITH 'Informe a Data de Emissão'.
      ADD 1 TO v_erro.
      EXIT.
    ENDIF.

    IF ( p_netwr IS INITIAL ).
      MESSAGE s836 WITH 'Informe o valor da NF-e'.
      ADD 1 TO v_erro.
      EXIT.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_nfenum
      IMPORTING
        output = p_nfenum.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_series
      IMPORTING
        output = p_series.

    CONCATENATE  p_nfenum  '-' p_series INTO v_xblnr.

    v_lifnr = s_kunnr-low.

    v_nftype = 'ZD'.

*  IF P_DEPFEC IS NOT INITIAL.
*    V_NFTYPE = 'ZH'.
*  ENDIF.

    CALL FUNCTION 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
      EXPORTING
        p_lifnr    = v_lifnr
        p_nftype   = v_nftype
        p_xblnr    = v_xblnr
        p_data     = p_docdat
        p_werks    = p_werks
        p_valor_nf = p_netwr
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
    IF sy-subrc NE 0.
      ADD 1 TO v_erro.
      CONCATENATE sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(_msg) SEPARATED BY space.
      MESSAGE _msg TYPE 'S'.
      EXIT.
    ENDIF.

  ELSE.
    " US - 81360 - inicio - CBRAND
    DATA: lva_prod_qtd_comerci TYPE j_1bnflin-menge,
          lva_quant            TYPE zsdt_export-quant,
          lva_docnum           TYPE j_1bnfdoc-docnum.

    IF ( p_chvnfe IS INITIAL ).
      MESSAGE s836 WITH 'Informe o número da NF-e'.
      ADD 1 TO v_erro.
      EXIT.
    ENDIF.
    SELECT *
   FROM zib_nfe_dist_ter INTO
   TABLE t_zib_nfe_dist_ter
  WHERE chave_nfe EQ p_chvnfe.

    IF t_zib_nfe_dist_ter IS INITIAL.
      MESSAGE s836 WITH 'Arquivo XML não recebido para NF-e/CT-e,' 'solicitar para Parceiro enviar para o email' 'nfe.fiscal@grupomaggi.com.br (cte.fiscal)'.
      ADD 1 TO v_erro.
      EXIT.
    ELSE.
      SELECT *
        FROM zib_nfe_dist_itm INTO
        TABLE t_zib_nfe_dist_itm
       WHERE chave_nfe EQ p_chvnfe.

      LOOP AT t_zib_nfe_dist_itm INTO DATA(w_zib_nfe_dist_itm).
        IF w_zib_nfe_dist_itm-prod_und_comerci = 'TO'.
          lva_prod_qtd_comerci = lva_prod_qtd_comerci + ( w_zib_nfe_dist_itm-prod_qtd_comerci * 1000 ).
        ELSE.
          lva_prod_qtd_comerci = lva_prod_qtd_comerci + ( w_zib_nfe_dist_itm-prod_qtd_comerci ).
        ENDIF.
      ENDLOOP.

      IF lva_prod_qtd_comerci < p_quant.
        MESSAGE s836 WITH 'NFe de Devolução menor que a quantidade informada'.
        ADD 1 TO v_erro.
        EXIT.
      ENDIF.

      READ TABLE t_zib_nfe_dist_ter INTO DATA(lwa_nfe_dist_ter) INDEX 1.

      p_docdat = lwa_nfe_dist_ter-dt_emissao.
      p_netwr  = lwa_nfe_dist_ter-vl_total.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lwa_nfe_dist_ter-numero
        IMPORTING
          output = p_nfenum.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lwa_nfe_dist_ter-serie
        IMPORTING
          output = p_series.

      CONCATENATE  p_nfenum  '-' p_series INTO v_xblnr.

      v_lifnr = s_kunnr-low.
      v_nftype = 'ZD'.

      CALL METHOD zcl_fiscal=>get_documento_chave
        EXPORTING
          i_chave   = p_chvnfe
          i_propria = abap_false
        RECEIVING
          r_docnum  = DATA(lc_docnum)
        EXCEPTIONS
          erro      = 1
          OTHERS    = 2.

      SELECT *
        FROM zsdt_export INTO
        TABLE t_zsdt_export
       WHERE docnum EQ lc_docnum .

      IF t_zsdt_export IS NOT INITIAL.
        LOOP AT t_zsdt_export INTO DATA(w_zsdt_export).
          lva_quant = w_zsdt_export-quant + lva_quant.
        ENDLOOP.

        lva_quant = lva_prod_qtd_comerci - lva_quant.

        IF p_quant > lva_quant.
          MESSAGE s836 WITH 'Quantidade Informada' p_quant 'não possui saldo para devolução'.
          ADD 1 TO v_erro.
          EXIT.
        ENDIF.
      ENDIF.

    ENDIF.
    CLEAR: lwa_nfe_dist_ter.
    " US - 81360 - fim - CBRAND
  ENDIF.

ENDFORM.

FORM f_check_lcto_terc CHANGING c_error.

  DATA: v_ktokd TYPE kna1-ktokd,
        v_stcd1 TYPE kna1-stcd1.

  CLEAR: s_nf_terc.

  SELECT SINGLE ktokd stcd1
    FROM kna1 INTO ( v_ktokd, v_stcd1 )
  WHERE  kunnr EQ s_kunnr-low.

  IF ( sy-subrc NE 0 ) OR ( s_kunnr-low IS INITIAL ).
    ADD 1 TO c_error.
    MESSAGE e836 WITH TEXT-006.
    EXIT.
  ENDIF.

  "Check Cliente Intercompany
  IF v_ktokd NE 'ZCIC'.
    s_nf_terc-check = 'X'.
  ELSE.
    CLEAR: p_nfenum, p_series,p_docdat,p_netwr.
  ENDIF.

ENDFORM.

*FORM Z_RFL_PRE_SELECIONADAS .
*
*  DATA: V_NETPR_1 TYPE J_1BNFLIN-NETPR,
*        V_NETPR_2 TYPE J_1BNFLIN-NETPR,
*        V_MENGE   TYPE J_1BNFLIN-MENGE.
*
*  CHECK IT_RFL_ZSDT0008[] IS NOT INITIAL.
*
*  LOOP AT T_LIN ASSIGNING FIELD-SYMBOL(<FS_LIN>).
*    DATA(_TABIX) = SY-TABIX.
*    DATA(_DEL) = ABAP_FALSE.
*
*    CLEAR: V_NETPR_1, V_NETPR_2.
*    IF ( <FS_LIN>-MENGE > 0 ).
*      IF ( <FS_LIN>-NETWRT > 0 ).
*        V_NETPR_1 = <FS_LIN>-NETWRT / <FS_LIN>-MENGE.
*      ENDIF.
*      IF ( <FS_LIN>-NETWR > 0 ).
*        V_NETPR_2 = <FS_LIN>-NETWR / <FS_LIN>-MENGE.
*      ENDIF.
*    ENDIF.
*
*    READ TABLE IT_RFL_ZSDT0008 WITH KEY DOCNUM_VINC = <FS_LIN>-DOCNUM.
*    IF ( SY-SUBRC NE 0 ) OR ( V_NETPR_1 <= 0 ) OR ( V_NETPR_2 <= 0 ).
*      DELETE T_LIN INDEX _TABIX.
*    ELSE.
*      CLEAR: V_MENGE.
*      LOOP AT IT_RFL_ZSDT0008 WHERE DOCNUM_VINC = <FS_LIN>-DOCNUM.
*        ADD IT_RFL_ZSDT0008-PESO_LIQ TO V_MENGE.
*      ENDLOOP.
*
*      IF ( V_MENGE > <FS_LIN>-MENGE ).
*        DELETE T_LIN INDEX _TABIX.
*      ELSE.
*        <FS_LIN>-MENGE  = V_MENGE.
*        <FS_LIN>-NETWRT = V_MENGE * V_NETPR_1.
*        <FS_LIN>-NETWR  = V_MENGE * V_NETPR_2.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.

FORM z_registro_cct.

  DATA: it_zsdt0001_ro_vinc  TYPE zsdt0001_ro_vinc_t.

  DATA: wl_zlest0146 TYPE zlest0146,
        lt_zlest0147 TYPE zlest0147_t,
        lt_zlest0168 TYPE zlest0168_t,
        v_doc_rateio TYPE char01.

  DATA: v_netpr_1 TYPE j_1bnflin-netpr,
        v_netpr_2 TYPE j_1bnflin-netpr,
        v_menge   TYPE j_1bnflin-menge.

  "Não tem um remessa de Lote pré selecionada
  CHECK it_rfl_zsdt0008[] IS INITIAL.

  CASE abap_true.
    WHEN p_allcct.
    WHEN p_comcct OR p_semcct.

      LOOP AT t_lin ASSIGNING FIELD-SYMBOL(<fs_lin>).

        DATA(_tabix)        = sy-tabix.
        DATA(_registro_cct) = abap_false.
        DATA(_del)          = abap_false.

        CLEAR: v_netpr_1, v_netpr_2.
        IF ( <fs_lin>-menge > 0 ).
          IF ( <fs_lin>-netwrt > 0 ).
            v_netpr_1 = <fs_lin>-netwrt / <fs_lin>-menge.
          ENDIF.
          IF ( <fs_lin>-netwr > 0 ).
            v_netpr_2 = <fs_lin>-netwr / <fs_lin>-menge.
          ENDIF.
        ENDIF.

        "Não pode Trazer RFL de Romaneio de Saida vinculado com Romaneio de Entrada
*        CALL FUNCTION 'ZROMANEIO_VINCULADO_NF'
*          EXPORTING
*            I_DOCNUM            = <FS_LIN>-DOCNUM
*            I_CK_VINC_ZMEMO00   = ABAP_TRUE
*          IMPORTING
*            E_ZSDT0001_RO_VINC  = IT_ZSDT0001_RO_VINC.
*
*        IF IT_ZSDT0001_RO_VINC[] IS NOT INITIAL.
*          _DEL = ABAP_TRUE.
*        ENDIF.

        "Check se Documento está registrado no CCT
        CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
          EXPORTING
            i_docnum            = <fs_lin>-docnum
            i_itmnum            = <fs_lin>-itmnum
            i_set_peso_disp_uso = abap_true
          IMPORTING
            e_zlest0146         = wl_zlest0146
            e_zlest0147         = lt_zlest0147
            e_zlest0168         = lt_zlest0168
            e_doc_rateio        = v_doc_rateio.

        IF ( wl_zlest0146 IS NOT INITIAL ) AND ( v_doc_rateio IS INITIAL ).

          CLEAR: v_menge.
          v_menge = wl_zlest0146-peso_disponivel_uso.

          IF ( v_menge > <fs_lin>-menge ).
            _del = abap_true.
          ELSE.
            <fs_lin>-menge  = v_menge.
            <fs_lin>-netwrt = v_menge * v_netpr_1.
            <fs_lin>-netwr  = v_menge * v_netpr_2.
          ENDIF.

          <fs_lin>-dtachegada = wl_zlest0146-dt_recepcao.

          _registro_cct = abap_true.
        ENDIF.

        CASE abap_true.
          WHEN p_comcct.
            IF _registro_cct IS INITIAL.
              _del = abap_true.
            ENDIF.

            SELECT SINGLE *
              FROM zsdt0168 INTO @DATA(_wl_0168)
             WHERE codigo_ra EQ @wl_zlest0146-local_codigo_ra.

            IF sy-subrc NE 0.
              _del = abap_true.
            ELSE.
              IF _wl_0168-lifnr NOT IN s_parc[].
                _del = abap_true.
              ENDIF.
            ENDIF.
            " IF ( P_NRDUE-LOW IS NOT INITIAL ) AND ( WL_ZLEST0146-LOCAL_CODIGO_RA NE P_RAEMB-LOW ).
            "   _DEL = ABAP_TRUE.
            " ENDIF.
          WHEN p_semcct.
            IF _registro_cct IS NOT INITIAL.
              _del = abap_true.
            ENDIF.

            "Valida Dados CCT Contrapartida
            IF _del EQ abap_false.
              PERFORM z_valida_cct_prod USING <fs_lin>
                                     CHANGING _del.
            ENDIF.
        ENDCASE.

        IF _del IS NOT INITIAL.
          DELETE t_lin INDEX _tabix.
        ENDIF.
      ENDLOOP.
  ENDCASE.

ENDFORM.

FORM z_valida_cct_prod USING p_nf_alv TYPE type_lin
                    CHANGING p_del TYPE c.

  DATA: v_rom_completo      TYPE char01,
        v_cct_cp            TYPE char01,
        wl_zlest0146_cp     TYPE zlest0146,
        it_zsdt0001_ro_vinc TYPE zsdt0001_ro_vinc_t.

  CHECK ( p_semcct EQ abap_true ) AND ( p_cct_cp EQ abap_true ).

  CALL FUNCTION 'ZROMANEIO_VINCULADO_NF'
    EXPORTING
      i_docnum            = p_nf_alv-docnum
      i_ck_cct_cp         = abap_true
      i_ck_cfop_e_zmemo00 = abap_true
    IMPORTING
      e_zsdt0001_ro_vinc  = it_zsdt0001_ro_vinc
      e_cct_cp            = v_cct_cp
      e_zlest0146_cp      = wl_zlest0146_cp.

  IF ( it_zsdt0001_ro_vinc[] IS INITIAL ). "Não for Romaneio Completo
    p_del = abap_true.
    RETURN.
  ENDIF.

  READ TABLE it_zsdt0001_ro_vinc INTO DATA(_wl_rom_vinc) INDEX 1.

  IF ( sy-subrc NE 0 ) OR ( _wl_rom_vinc-docnum_vinc IS INITIAL ).
    p_del = abap_true.
    RETURN.
  ENDIF.

  IF ( v_cct_cp EQ abap_false ) OR ( wl_zlest0146_cp IS INITIAL ).
    p_del = abap_true.
  ELSE.
    SELECT SINGLE *
      FROM zsdt0168 INTO @DATA(_wl_0168)
     WHERE codigo_ra EQ @wl_zlest0146_cp-local_codigo_ra.

    IF sy-subrc NE 0.
      p_del = abap_true.
    ELSE.
      IF _wl_0168-lifnr NOT IN s_parc[].
        p_del = abap_true.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_LOGISTICA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_logistica .

*  SELECT L~DOCNUM L~ITMNUM L~MATNR L~MAKTX
*         L~BWKEY  L~MATKL  L~NBM
*         L~TAXSIT L~TAXSI2 L~MATUSE
*         L~REFKEY N~MENGE  N~MEINS
*         L~NETPR  L~NETWR  L~NETWRT
*         L~TAXLW1 L~TAXLW2 L~ITMTYP
*         L~WERKS  L~TAXLW3 L~TAXLW4
*    INTO TABLE T_LIN
*    FROM J_1BNFLIN AS L
*   INNER JOIN ZLEST0172 AS N ON N~ID_NOMEACAO_TRAN EQ S_NOM-LOW AND N~DOCNUM EQ L~DOCNUM
*  WHERE L~CHARG  IN S_SAFRA
*    AND L~WERKS  EQ P_WERKS
*    AND L~MATNR  IN S_MATNR
*    AND L~CFOP   EQ '5905AA'.
*
*  LOOP AT T_LIN ASSIGNING FIELD-SYMBOL(<FS_LIN>).
*    <FS_LIN>-MEINS = 'KG'.
*  ENDLOOP.

ENDFORM.

MODULE pbo_0101 OUTPUT.

  SET PF-STATUS 'PF0101'.

  IF obj_alv_0101 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0101'.

    IF obj_container_0101 IS INITIAL.
      CREATE OBJECT obj_container_0101
        EXPORTING
          container_name = 'CC_ALV_0101'.
    ENDIF.

    CREATE OBJECT obj_alv_0101
      EXPORTING
        i_parent = obj_container_0101.

    gs_layout-zebra       = 'X'.
    gs_layout-sel_mode    = 'A'.
    gs_variant-report     = sy-repid.

    PERFORM f_exclude_fcode USING '0101'.

    CALL METHOD obj_alv_0101->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0101.

    CALL METHOD obj_alv_0101->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_0101->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD obj_alv_0101->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.



  ELSE.
    CALL METHOD obj_alv_0101->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.



FORM f_refresh_objetos .

  CLEAR: gs_layout,
         gs_variant.

  REFRESH: it_exclude_fcode.

ENDFORM.

FORM f_criar_catalog USING p_screen.

  FREE: wa_fcat, it_fcat.

  CASE p_screen.
    WHEN '0101'.

      PERFORM f_estrutura_alv USING:

         01  ''           ''             'IT_SAIDA_0101' 'CHAVE_NFE'     'Chave NF-e'          '44'   'X'    ''  ' ' ' ' ' ' ' ' ' ',
         02  'J_1BNFLIN'  'MENGE'        'IT_SAIDA_0101' 'MENGE'         'Quantidade(KG)'      '14'   'X'    'X' ' ' ' ' ' ' ' ' ' '.

  ENDCASE.

ENDFORM.

FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                           VALUE(p_tabname)       LIKE dd02d-tabname
                           VALUE(p_field)         LIKE dd03d-fieldname
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                           VALUE(p_outputlen)
                           VALUE(p_edit)
                           VALUE(p_sum)
                           VALUE(p_emphasize)
                           VALUE(p_just)
                           VALUE(p_hotspot)
                           VALUE(p_f4)
                           VALUE(p_checkbox).

  CLEAR wa_fcat.

  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = ' '.
  wa_fcat-edit        = p_edit.
  wa_fcat-col_pos     = p_col_pos.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-no_out      = ' '.
  wa_fcat-do_sum      = p_sum.
  wa_fcat-reptext     = p_scrtext_l.
  wa_fcat-scrtext_s   = p_scrtext_l.
  wa_fcat-scrtext_m   = p_scrtext_l.
  wa_fcat-scrtext_l   = p_scrtext_l.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-style       =
  wa_fcat-just        = p_just.
  wa_fcat-hotspot     = p_hotspot.
  wa_fcat-f4availabl  = p_f4.
  wa_fcat-checkbox    = p_checkbox.

  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV

FORM f_exclude_fcode USING p_screen.

  APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_check             TO it_exclude_fcode.

ENDFORM.

MODULE pai_0101 INPUT.

  DATA: it_nfe_cons_portal TYPE zde_chave_doc_e_t,
        wl_nfe_cons_portal TYPE zde_chave_doc_e,
        wl_ret_ter         TYPE zsdt_retlote_ter.

  DATA: v_menge_vinc TYPE j_1bnflin-menge,
        v_menge_01   TYPE c LENGTH 50,
        v_menge_02   TYPE c LENGTH 50.

  DATA: v_cpf_cnpj_prod TYPE c LENGTH 14.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      CLEAR: it_nfe_cons_portal[], it_ret_ter[], v_menge_vinc, v_cpf_cnpj_prod.

      IF it_saida_0101[] IS INITIAL.
        MESSAGE 'Nenhuma NF-e foi informada!' TYPE 'I'.
        RETURN.
      ENDIF.

      IF p_idprd IS INITIAL.
        MESSAGE 'Parâmetro codigo Produtor não informado!' TYPE 'I'.
        RETURN.
      ENDIF.

      SELECT SINGLE *
        FROM lfa1 INTO @DATA(wl_lfa1_prod)
       WHERE lifnr EQ @p_idprd.

      IF sy-subrc NE 0.
        MESSAGE |Cadastro fornecedor: { p_idprd } não encontrado!| TYPE 'I'.
        RETURN.
      ENDIF.

      IF wl_lfa1_prod-stcd1 IS NOT INITIAL.
        v_cpf_cnpj_prod = wl_lfa1_prod-stcd1.
      ELSE.
        v_cpf_cnpj_prod = wl_lfa1_prod-stcd2.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = v_cpf_cnpj_prod
        IMPORTING
          output = v_cpf_cnpj_prod.

      LOOP AT it_saida_0101 INTO DATA(wl_saida_0001).

        CLEAR: wl_nfe_cons_portal.

        IF strlen( wl_saida_0001-chave_nfe ) NE 44.
          MESSAGE 'Existem notas com chave NF-e inválida!' TYPE 'I'.
          RETURN.
        ENDIF.

        IF wl_saida_0001-menge <= 0.
          MESSAGE |Não informado quantidade para a chave: { wl_saida_0001-chave_nfe } !| TYPE 'I'.
          RETURN.
        ENDIF.

        IF v_cpf_cnpj_prod NE  wl_saida_0001-chave_nfe+6(14).
          MESSAGE |Chave: { wl_saida_0001-chave_nfe } não emitida pelo Forncedor Código: { p_idprd } !| TYPE 'I'.
          "RETURN.
        ENDIF.

        wl_nfe_cons_portal = wl_saida_0001-chave_nfe.

        ADD wl_saida_0001-menge TO v_menge_vinc.

        APPEND wl_nfe_cons_portal TO it_nfe_cons_portal.

      ENDLOOP.

      "Checar NF-e Duplicada
      DATA(_it_saida_aux) = it_saida_0101[].
      SORT _it_saida_aux BY chave_nfe.
      DELETE ADJACENT DUPLICATES FROM _it_saida_aux COMPARING chave_nfe.

      IF lines( _it_saida_aux ) NE lines( it_saida_0101[] ).
        MESSAGE 'Existem notas duplicadas!' TYPE 'I'.
        RETURN.
      ENDIF.

      "Validar quantidade de notas informadas..
      WRITE v_menge_vinc TO v_menge_01.
      WRITE p_quant      TO v_menge_02.

      CONDENSE: v_menge_01, v_menge_02 NO-GAPS.

      IF v_menge_vinc NE p_quant.
        MESSAGE |Quantidade informada de notas: { v_menge_01 } diferente da quantidade do retorno: { v_menge_02 } !| TYPE 'I'.
        RETURN.
      ENDIF.

      "Validar se notas possuem CCT
      DATA(_erro_autenticacao) = abap_false.
      CALL FUNCTION 'ZCCT_CONFIRMA_REC_NF_PORTAL'
        EXPORTING
          i_chaves            = it_nfe_cons_portal[]
        IMPORTING
          e_erro_autenticacao = _erro_autenticacao.

      CHECK _erro_autenticacao EQ abap_false.

      LOOP AT it_saida_0101 INTO wl_saida_0001.
        SELECT SINGLE *
          FROM zlest0186 INTO @DATA(_wl_zlest0168)
         WHERE chave EQ @wl_saida_0001-chave_nfe.

        IF ( sy-subrc NE 0 ).
          MESSAGE i136(zcct) WITH wl_saida_0001-chave_nfe.
          RETURN.
        ENDIF.
      ENDLOOP.

      LOOP AT it_saida_0101 INTO DATA(wl_saida_0101).

        CLEAR: wl_ret_ter.

        wl_ret_ter-chave_nfe     = wl_saida_0101-chave_nfe.
        wl_ret_ter-quant_vinc    = wl_saida_0101-menge.
        wl_ret_ter-bukrs         = p_bukrs.
        wl_ret_ter-werks         = p_werks.
        wl_ret_ter-safra         = s_safra-low.
        wl_ret_ter-lgort         = s_lgort-low.
        wl_ret_ter-lifnr_z1      = s_parc-low.
        wl_ret_ter-matnr         = s_matnr-low.
        wl_ret_ter-registro_cct  = abap_true.
        wl_ret_ter-lifnr_emissor = wl_lfa1_prod-lifnr.
        wl_ret_ter-cnpj_emissor  = wl_lfa1_prod-stcd1(14).
        wl_ret_ter-cpf_emissor   = wl_lfa1_prod-stcd2(11).
        wl_ret_ter-ie_emissor    = wl_lfa1_prod-stcd3.
        wl_ret_ter-dt_registro   = sy-datum.
        wl_ret_ter-hr_registro   = sy-uzeit.
        wl_ret_ter-us_registro   = sy-uname.

        APPEND wl_ret_ter TO it_ret_ter.

      ENDLOOP.

      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

FORM z_check_saldo_docs.

  DATA: v_qtd_vinc TYPE zsdt_retlote-quant_vinc.

  LOOP AT t_lin ASSIGNING FIELD-SYMBOL(<fs_lin>).
    CLEAR: v_qtd_vinc.

    LOOP AT t_ret INTO DATA(wl_retlote) WHERE docnum EQ <fs_lin>-docnum.
      ADD wl_retlote-quant_vinc TO v_qtd_vinc.
    ENDLOOP.

    IF v_qtd_vinc >= <fs_lin>-menge.
      <fs_lin>-del = abap_true.
    ENDIF.
  ENDLOOP.

  DELETE t_lin WHERE del = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_next_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7995   text
*      -->P_7996   text
*      <--P_VL_NRONC  text
*----------------------------------------------------------------------*
FORM get_next_number  USING    p_object   "TYPE nrobj
                               p_nr_range "TYPE nrnr
                      CHANGING p_number.

  CLEAR p_number.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = p_nr_range
      object                  = p_object
    IMPORTING
      number                  = p_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc NE 0.
    CLEAR: p_number.
    MESSAGE e836(sd) WITH 'O intervalo de numeração,'
                      'não foi encontrado!'.
    RETURN.
  ENDIF.

ENDFORM.                    " get_next_number
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONA_TVARV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_seleciona_tvarv .
  DATA: w_final LIKE LINE OF rg_final.

  CONSTANTS: c_name TYPE rvari_vnam VALUE 'Z_FIN_ESTORN_ZLES0050'.

  SELECT * INTO TABLE tg_tvarvc
    FROM tvarvc
    WHERE name = c_name.
  IF sy-subrc EQ 0.
    rg_final = VALUE #( FOR l IN tg_tvarvc ( sign = 'I' option = 'EQ' low = l-low ) ).
  ENDIF.
ENDFORM.

FORM f_descarta_docs_vinc_1x1  TABLES p_documentos STRUCTURE t_lin.

  CHECK p_documentos[] IS NOT INITIAL.

  SELECT *
    FROM zsdtvinc_p_flote INTO TABLE @DATA(lit_zsdtvinc_p_flote)
    FOR ALL ENTRIES IN @p_documentos
  WHERE docnum_flote EQ @p_documentos-docnum.

  SELECT *
    FROM zsdtvinc_p_flote APPENDING TABLE @lit_zsdtvinc_p_flote
    FOR ALL ENTRIES IN @p_documentos
  WHERE docnum_eprod EQ @p_documentos-docnum.

  CHECK lit_zsdtvinc_p_flote[] IS NOT INITIAL.

  " >>> Remover registros cancelados - SMC 05-06-2025 ISSUE 181708 >>>
  DELETE lit_zsdtvinc_p_flote WHERE cancel = 'X'.
 " <<< Remover registros cancelados - SMC 05-06-2025 ISSUE 181708 <<<

  LOOP AT p_documentos ASSIGNING FIELD-SYMBOL(<fs_document>).
    READ TABLE lit_zsdtvinc_p_flote WITH KEY docnum_flote = <fs_document>-docnum TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      CLEAR  <fs_document>-docnum.
    ENDIF.

    READ TABLE lit_zsdtvinc_p_flote WITH KEY docnum_eprod = <fs_document>-docnum TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      CLEAR  <fs_document>-docnum.
    ENDIF.
  ENDLOOP.

  DELETE p_documentos WHERE docnum IS INITIAL.

ENDFORM.
