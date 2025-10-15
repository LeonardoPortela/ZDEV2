*&---------------------------------------------------------------------*
*& Report  ZWRR0009
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zwrr0009.

TABLES: zfiwrt0021, zfiwrt0022.



TYPES: BEGIN OF ty_saida,
         status              TYPE icon-id,
         contrato            TYPE zfiwrt0021-contrato,
         tipo_contrato       TYPE char15,
         kunnr               TYPE zfiwrt0021-kunnr,
         name1               TYPE kna1-name1,
         bukrs               TYPE zfiwrt0021-bukrs,
         butxt               TYPE t001-butxt,
         texto_nota          TYPE zfiwrt0021-texto_nota,
         operacao            TYPE zfiwrt0021-operacao,
         tarifa              TYPE zfiwrt0021-tarifa01,
         montante            TYPE zfiwrt0021-montante01,
         vlr_faturado        TYPE zfiwrt0021-tarifa01,
         seq_lcto            TYPE zfiwrt0022-seq_lcto,
         seq_lcto_flag(20),
         docnum              TYPE zfiwrt0008-docnum,
         docnum_flag(20),
         doc_contab_flag(20),
         doc_contab(10),
         nfenum              TYPE j_1bnfdoc-nfenum,
         nfnum_flag(20),
         ebeln               TYPE ekko-ebeln,
         ebeln_flag(20),
         migo_flag(20),
         se_recordid         TYPE zib_nfe_dist_ter-se_recordid,
         branch              TYPE zfiwrt0021-branch,
         matnr               TYPE zfiwrt0021-matnr,
         mes                 TYPE zfiwrt0022-mes,
         ano                 TYPE zfiwrt0022-ano,
         docs_estornados     TYPE zfiwrt0008-docs_estornados,
         banco               TYPE zfiwrt0021-banco,
         agencia_banc        TYPE zfiwrt0021-agencia_banc,
         conta_banc          TYPE zfiwrt0021-conta_banc,
         vencimento          TYPE zfiwrt0021-vencimento,
         vencimento_f        TYPE zfiwrt0021-vencimento_f,
         pgto_mes_seguinte   TYPE zfiwrt0021-pgto_mes_seguinte,
         fatura_atu          TYPE zfiwrt0021-fatura_atu,
         fatura_f            TYPE zfiwrt0021-fatura_f,
         fatura_u            TYPE zfiwrt0021-fatura_u,
         cancel(1),
         docsta              TYPE j_1bnfe_active-docsta,
         message             TYPE bapi_msg.
TYPES:   color             TYPE   kkblo_specialcol OCCURS 0.
TYPES:   END OF ty_saida.

TYPES: BEGIN OF ty_editor,
         line(72).
TYPES:   END   OF ty_editor.


DATA: BEGIN OF tg_mensagens OCCURS 0,
        seqnum  TYPE  zfiwrt0005-seqnum,
        linnum  TYPE  zfiwrt0005-linnum,
        message TYPE  zfiwrt0005-message,
      END OF  tg_mensagens.

DATA: BEGIN OF tg_itens OCCURS 0,
        mark(1),
        itmnum  TYPE zfiwrt0009-itmnum,
        matnr   TYPE zfiwrt0009-matnr,
        maktx   TYPE makt-maktx,
        cfop    TYPE zfiwrt0006-cfop,
        charg   TYPE zfiwrt0009-charg,
        werks   TYPE t001w-werks,
        lgort   TYPE zfiwrt0009-lgort,
        menge   TYPE zfiwrt0009-menge,
        meins   TYPE zfiwrt0009-meins,
        netpr   TYPE zfiwrt0009-netpr,
        netwr   TYPE zfiwrt0009-netwr,
        anln1   TYPE zfiwrt0009-anln1,
        anln2   TYPE zfiwrt0009-anln2,
        steuc   TYPE marc-steuc,
      END OF tg_itens.

DATA: BEGIN OF tg_impo OCCURS 0,
        taxtyp   TYPE zfiwrt0010-taxtyp,
        ttypetxt TYPE j_1bttytxt,
        taxgrp   TYPE j_1btaxgrp,
        base     TYPE zfiwrt0010-base,
        rate     TYPE zfiwrt0010-rate,
        taxval   TYPE zfiwrt0010-taxval,
        excbas   TYPE zfiwrt0010-excbas,
        othbas   TYPE zfiwrt0010-othbas,
        line     TYPE i,  " Alteracao feita por Alexandre ref; CS1016278 - IR107256 19.04.2023
      END OF  tg_impo.

**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003

DATA: BEGIN OF tg_impo2 OCCURS 0,

        line     TYPE i,
        taxtyp   TYPE zfiwrt0010-taxtyp,
        ttypetxt TYPE j_1bttytxt,
        taxgrp   TYPE j_1btaxgrp,
        base     TYPE zfiwrt0010-base,
        rate     TYPE zfiwrt0010-rate,
        taxval   TYPE zfiwrt0010-taxval,
        excbas   TYPE zfiwrt0010-excbas,
        othbas   TYPE zfiwrt0010-othbas,
      END OF tg_impo2.

** fim alteração Alexandre  ref; CS1016278 - IR107256 - 27.02.2003

DATA:BEGIN OF tg_impo_comp OCCURS 0,
       itmnum TYPE zfiwrt0010-itmnum.
       INCLUDE STRUCTURE tg_impo.
DATA: END OF tg_impo_comp.

DATA: BEGIN OF tg_contab OCCURS 0,
        bschl   TYPE zfiwrt0011-bschl,
        hkont   TYPE zfiwrt0011-hkont,
        txt50   TYPE skat-txt50,
        dmbtr   TYPE zfiwrt0011-dmbtr,
        taxtyp  TYPE zfiwrt0011-taxtyp,
        estorno TYPE zfiwrt0011-estorno,
        newbw   TYPE zfiwrt0011-newbw,
        zlsch   TYPE zfiwrt0011-zlsch,
        zfbdt   TYPE zfiwrt0011-zfbdt,
        kostl   TYPE zfiwrt0011-kostl,
        umskz   TYPE zfiwrt0011-umskz,
        vbund   TYPE zfiwrt0011-vbund,
        style2  TYPE lvc_t_styl,
      END OF tg_contab.

DATA: BEGIN OF tg_movest OCCURS 0,
        bwart   TYPE zfiwrt0004-bwart,
        tcode   TYPE zfiwrt0004-tcode,
        mwskz1  TYPE zfiwrt0004-mwskz1,
        estorno TYPE zfiwrt0004-estorno,
      END OF tg_movest.

TYPES: BEGIN OF ty_parc,
         parvw    TYPE zfiwrt0015-parvw,
         parid    TYPE zfiwrt0015-parid,
         nome(80),
         style    TYPE lvc_t_styl,
       END OF ty_parc.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

DATA: t_fieldcatalog TYPE lvc_t_fcat,
      w_fieldcatalog TYPE lvc_s_fcat,
      wa_layout      TYPE lvc_s_layo,
      wa_stable      TYPE lvc_s_stbl,
      estrutura      TYPE TABLE OF ty_estrutura,
      wa_estrutura   TYPE ty_estrutura,
      wg_indic_cont  TYPE sy-tabix.



DATA: it_saida            TYPE TABLE OF ty_saida,
      wa_saida            TYPE ty_saida,
      it_saida_aux        TYPE TABLE OF ty_saida,
      wa_saida_aux        TYPE ty_saida,
      it_zfiwrt0001       TYPE TABLE OF zfiwrt0001,
      wa_zfiwrt0001       TYPE zfiwrt0001,
      it_zfiwrt0002       TYPE TABLE OF zfiwrt0002,
      wa_zfiwrt0002       TYPE zfiwrt0002,
      it_zfiwrt0003       TYPE TABLE OF zfiwrt0003,
      wa_zfiwrt0003       TYPE zfiwrt0003,
      it_zfiwrt0004       TYPE TABLE OF zfiwrt0004,
      wa_zfiwrt0004       TYPE zfiwrt0004,
      it_zfiwrt0005       TYPE TABLE OF zfiwrt0005,
      wa_zfiwrt0005       TYPE zfiwrt0005,
      it_zfiwrt0006       TYPE TABLE OF zfiwrt0006,
      wa_zfiwrt0006       TYPE zfiwrt0006,
      it_zfiwrt0007       TYPE TABLE OF zfiwrt0007,
      wa_zfiwrt0007       TYPE zfiwrt0007,
      it_zfi0007          TYPE TABLE OF zfiwrt0007,
      wa_zfi0007          TYPE zfiwrt0007,
      it_zfiwrt0008       TYPE TABLE OF zfiwrt0008,
      wa_zfiwrt0008       TYPE zfiwrt0008,
      it_zfiwrt8          TYPE TABLE OF zfiwrt0008,
      wa_zfiwrt8          TYPE zfiwrt0008,
      it_zfiwrt0009       TYPE TABLE OF zfiwrt0009,
      wa_zfiwrt0009       TYPE zfiwrt0009,
      it_zfiwrt0010       TYPE TABLE OF zfiwrt0010,
      wa_zfiwrt0010       TYPE zfiwrt0010,
      it_zfiwrt0011       TYPE TABLE OF zfiwrt0011,
      wa_zfiwrt0011       TYPE zfiwrt0011,
      it_zfiwrt0012       TYPE TABLE OF zfiwrt0012,
      wa_zfiwrt0012       TYPE  zfiwrt0012,
      it_zfiwrt0013       TYPE TABLE OF zfiwrt0013,
      wa_zfiwrt0013       TYPE  zfiwrt0013,
      it_zfiwrt0015       TYPE TABLE OF zfiwrt0015,
      wa_zfiwrt0015       TYPE  zfiwrt0015,
      it_zfiwrt0021       TYPE TABLE OF zfiwrt0021,
      wa_zfiwrt0021       TYPE zfiwrt0021,
      it_zfiwrt0022       TYPE TABLE OF zfiwrt0022,
      wa_zfiwrt0022       TYPE zfiwrt0022,
      it_zfiwrt22         TYPE TABLE OF zfiwrt0022,
      wa_zfiwrt22         TYPE zfiwrt0022,
      it_zfiwrt1000       TYPE TABLE OF  zfiwrt1000,
      wa_zfiwrt1000       TYPE zfiwrt1000,
      it_estra            TYPE TABLE OF zfi_estrategia_imp,
      wa_estra            TYPE zfi_estrategia_imp,
      it_lotes            TYPE TABLE OF zfi_lotes_imp,
      it_docs             TYPE TABLE OF zgl_docs_imp,
      it_docs_energia     TYPE TABLE OF zgl_docs_energia,

      it_estra_tot        TYPE TABLE OF zfi_estrategia_imp,
      wa_estra_tot        TYPE zfi_estrategia_imp,
      tl_1000             TYPE TABLE OF zfiwrt1000,
      wl_1000             TYPE zfiwrt1000,
*
      it_icon             TYPE TABLE OF icon,
      wa_icon             TYPE icon,
*
      tl_0008             TYPE TABLE OF zfiwrt0008 WITH HEADER LINE,
      tl_zib_chv          TYPE TABLE OF zib_contabil_chv WITH HEADER LINE,
      tl_zib_err          TYPE TABLE OF zib_contabil_err WITH HEADER LINE,
      tl_zib_cont         TYPE TABLE OF zib_contabil WITH HEADER LINE,
      tl_0011             TYPE TABLE OF zfiwrt0011 WITH HEADER LINE,

      it_j_1baj           TYPE TABLE OF j_1baj,
      wa_j_1baj           TYPE j_1baj,
      it_j_1bajt          TYPE TABLE OF j_1bajt,
      wa_j_1bajt          TYPE j_1bajt,
      it_tbsl             TYPE TABLE OF tbsl,
      wa_tbsl             TYPE tbsl,
      it_skat             TYPE TABLE OF skat,
      wa_skat             TYPE skat,
*      IT_CSKB             TYPE TABLE OF CSKB,
      wa_cskb             TYPE cskb,
*      IT_USER_ADDR        TYPE TABLE OF USER_ADDR,
*      WA_USER_ADDR        TYPE  USER_ADDR,
      it_mara             TYPE TABLE OF mara,
      wa_mara             TYPE mara,
      it_marc             TYPE TABLE OF marc,
      wa_marc             TYPE marc,
      it_t001             TYPE TABLE OF t001,
      wa_t001             TYPE t001,
      it_t001w            TYPE TABLE OF t001w,
      wa_t001w            TYPE  t001w,
      it_kna1             TYPE TABLE OF kna1,
      wa_kna1             TYPE kna1,
      wa_lfa1             TYPE lfa1,
      it_j_1baa           TYPE TABLE OF j_1baa,
      wa_j_1baa           TYPE j_1baa,
      tl_texto            TYPE catsxt_longtext_itab,
      wl_texto            TYPE LINE OF catsxt_longtext_itab,
      wl_texto_fiscal(30),
      wl_indcoper         TYPE zfiwrt0006-indcoper,
      wg_shipfrom         TYPE lfa1-regio,
      wg_shipto           TYPE lfa1-regio,
      tl_impo_aux         LIKE TABLE OF tg_impo WITH HEADER LINE,
      tg_parc             TYPE TABLE OF ty_parc  WITH HEADER LINE.

DATA: tl_docest    TYPE TABLE OF zfiwrs0003 WITH HEADER LINE.
DATA: wl_color TYPE kkblo_specialcol,
      BEGIN OF tl_infogrid,
        color TYPE kkblo_specialcol OCCURS 1,
      END OF tl_infogrid.

DATA: wl_cont    TYPE sy-tabix,
      wg_flag,
      p_seq_lcto TYPE zfiwrt0008-seq_lcto.

DATA: tl_bdc      TYPE TABLE OF bdcdata,
      wl_bdc      TYPE bdcdata,
      l_msg       TYPE char50,
      l_usnam     TYPE sy-uname,
      l_icon_name TYPE icon-name,
      v_zfbdt     TYPE zfiwrt0011-zfbdt.

CLASS lcl_timer DEFINITION DEFERRED.


DATA: g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,
      tl_function          TYPE ui_functions,
      wl_function          LIKE tl_function WITH HEADER LINE,
      tg_selectedcell      TYPE lvc_t_cell,
      wg_selectedcell      TYPE lvc_s_cell,
      tg_selectedrow       TYPE lvc_t_row,
      wg_selectedrow       TYPE lvc_s_row,
      it_fcat              TYPE TABLE OF lvc_s_fcat,
      wl_fcat              TYPE lvc_s_fcat,
      wa_layout01          TYPE lvc_s_layo,
      ty_toolbar           TYPE stb_button,
      wa_stable01          TYPE lvc_s_stbl,
      ob_recev             TYPE REF TO lcl_timer,
      ob_timer             TYPE REF TO cl_gui_timer,
      msg_id               LIKE t100-arbgb,
      msg_no               LIKE t100-msgnr,
      msg_var1             LIKE balm-msgv1,
      msg_var2             LIKE balm-msgv2,
      msg_var3             LIKE balm-msgv3,
      msg_var4             LIKE balm-msgv4,
      wmessage             TYPE bapi_msg.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_bukrs FOR zfiwrt0021-bukrs NO INTERVALS OBLIGATORY,
                  p_data  FOR zfiwrt0021-data_inicio NO-EXTENSION OBLIGATORY.  "Data Faturamento
  PARAMETERS: p_mes TYPE zfiwrt0022-mes   OBLIGATORY,
              p_ano TYPE zfiwrt0022-ano    OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_n_proc RADIOBUTTON GROUP g1 DEFAULT 'X',
              p_proc   RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b2.

PARAMETERS: r_ped   LIKE bsid-umskz AS CHECKBOX  DEFAULT ' '.

INITIALIZATION.
  p_data-low = sy-datum.
  APPEND p_data.

AT SELECTION-SCREEN OUTPUT.

  IF  p_data-low NE sy-datum AND p_n_proc EQ abap_true.
    MESSAGE 'Data de faturamento deve a data atual.' TYPE 'I'.
    SET CURSOR FIELD 'P_DATA-LOW' .
  ENDIF.

START-OF-SELECTION.

  IF  p_data-low NE sy-datum AND p_n_proc EQ abap_true.
    MESSAGE 'Data de faturamento deve ser maior que mês/ano a faturar ' TYPE 'I'.
  ELSE.
    PERFORM z_seleciona_dados.
    PERFORM z_tratar_dados.

    CALL SCREEN 0100.
  ENDIF.
CLASS lcl_timer DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_finished FOR EVENT finished OF cl_gui_timer.
ENDCLASS.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotsopt_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

ENDCLASS.


CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.
    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value.

    LOOP AT er_data_changed->mt_good_cells
                        INTO ls_good
                        WHERE fieldname = 'OPERACAO'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'OPERACAO'
          i_value     = lv_value.
    ENDLOOP.

  ENDMETHOD.

  METHOD on_hotsopt_click.

    DATA: vl_nfobjn TYPE j_1binterf-nfobjn,
          vl_docnum TYPE j_1bnfdoc-docnum,
          opt       TYPE ctu_params.

    CASE e_column_id.
      WHEN 'MIGO_FLAG'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
        IF wa_saida-migo_flag IS NOT INITIAL AND wa_saida-ebeln IS NOT INITIAL.
          SELECT SINGLE * FROM zfiwrt0008 INTO @DATA(wa_t0008)
                WHERE seq_lcto EQ @wa_saida-seq_lcto.
          IF wa_t0008-docnum IS NOT INITIAL.
            SELECT SINGLE *
            FROM j_1bnfe_active
             INTO @DATA(wl_active)
              WHERE docnum EQ @wa_t0008-docnum.
            IF sy-subrc = 0.
              CONCATENATE wl_active-regio wl_active-nfyear wl_active-nfmonth wl_active-stcd1
                  wl_active-model wl_active-serie  wl_active-nfnum9  wl_active-docnum9 wl_active-cdv
                  INTO DATA(v_chave_doc).
              SELECT SINGLE zfbdt
                INTO @DATA(vzfbdt)
                FROM zfiwrt0011
                WHERE seq_lcto = @wa_saida-seq_lcto.
              "
              SUBMIT zmmr118 WITH chaven EQ v_chave_doc
                             WITH pvenct EQ vzfbdt
                              AND RETURN.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN 'EBELN_FLAG'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
        IF wa_saida-ebeln IS NOT INITIAL.
          SET PARAMETER ID 'BES' FIELD wa_saida-ebeln.
          CALL TRANSACTION 'ME22N' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'SEQ_LCTO_FLAG'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

        SELECT * FROM  zfiwrt1000 INTO TABLE @DATA(tg_1000)
          WHERE field    EQ 'SEQ_LCTO'
          AND   seq_lcto EQ  @wa_saida-seq_lcto.

        IF tg_1000 IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES  tg_1000.

        ELSEIF wa_saida-seq_lcto IS NOT INITIAL.

          SET PARAMETER ID 'SEQ' FIELD  wa_saida-seq_lcto.
          CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

        ENDIF.
      WHEN 'DOCNUM_FLAG'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

        SELECT * FROM  zfiwrt1000 INTO TABLE tg_1000
          WHERE field    EQ 'DOCNUM'
          AND   seq_lcto EQ  wa_saida-seq_lcto.

        IF tg_1000 IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES  tg_1000.

        ELSEIF wa_saida-docnum IS NOT INITIAL.

          vl_docnum = wa_saida-docnum.

          CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
            EXPORTING
              doc_number         = vl_docnum
            IMPORTING
              obj_number         = vl_nfobjn
            EXCEPTIONS
              document_not_found = 1
              docum_lock         = 2
              OTHERS             = 3.

          CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
            EXPORTING
              obj_number         = vl_nfobjn
            EXCEPTIONS
              object_not_found   = 1
              scr_ctrl_not_found = 2
              OTHERS             = 3.
        ENDIF.
      WHEN 'DOC_CONTAB_FLAG'.
        REFRESH  tl_1000.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
        READ TABLE tl_0008 INTO tl_0008
          WITH KEY seq_lcto = wa_saida-seq_lcto.

        LOOP AT tl_zib_err INTO tl_zib_err
           WHERE obj_key EQ tl_0008-obj_key
             AND type    EQ 'E'.

          MOVE: tl_zib_err-type    TO wl_1000-type,
                tl_zib_err-message TO wl_1000-mensagem.

          APPEND wl_1000 TO tl_1000.
          CLEAR: wl_1000.
        ENDLOOP.
        IF tl_1000[] IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES tl_1000.

        ELSEIF wa_saida-doc_contab IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD  wa_saida-doc_contab.
          SET PARAMETER ID 'BUK' FIELD  wa_saida-bukrs.
          SET PARAMETER ID 'GJR' FIELD  p_data-low+0(4).

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'NFNUM_FLAG'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
        SELECT * FROM  zfiwrt1000 INTO TABLE tg_1000
        WHERE field    EQ 'NFENUM'
        AND   seq_lcto EQ  wa_saida-seq_lcto.


        IF tg_1000 IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES  tg_1000.

        ELSEIF wa_saida-docnum IS NOT INITIAL.
          REFRESH: tl_bdc.
          PERFORM f_preencher_dynpro USING:
                'X'   'Z_1BNFE_MONITOR'          '1000',
                ''    'DOCNUM-LOW'               wa_saida-docnum,
                ''    'USER-LOW'                 space,
                ''    'DATE0-LOW'                space,
                ''    'BUKRS-LOW'                wa_saida-bukrs,
                ''    'BDC_OKCODE'               'ONLI'.

          opt-dismode = 'E'.
          opt-defsize = ' '.
          opt-racommit = 'X'.

          CALL TRANSACTION 'ZNFE' USING tl_bdc OPTIONS FROM opt.
        ENDIF.
      WHEN 'CONTRATO'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
        SET PARAMETER ID 'CTR' FIELD  wa_saida-contrato.
        SET PARAMETER ID 'BUK' FIELD  wa_saida-bukrs.
        SET PARAMETER ID 'BRC' FIELD  wa_saida-branch.
        SET PARAMETER ID 'KUN' FIELD  wa_saida-kunnr.
        SET PARAMETER ID 'ANO' FIELD  wa_saida-ano.
        CALL TRANSACTION 'ZNFW0008' AND SKIP FIRST SCREEN.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_timer IMPLEMENTATION.
  METHOD handle_finished.

    PERFORM z_busca_dados_atualizado.

    wa_stable-row = abap_true.
    wa_stable-col = abap_true.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    CALL METHOD ob_timer->run.

  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.

  AUTHORITY-CHECK OBJECT 'ZNFW010'
    ID 'ACTVT'  FIELD '64'. "gerar
  IF sy-subrc <> 0.
    APPEND 'GERAR' TO fcode.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZNFW010'
    ID 'ACTVT'  FIELD '85'. "Estorno
  IF sy-subrc <> 0.
    APPEND 'ESTORNO' TO fcode.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZNFW010'
    ID 'ACTVT'  FIELD 'A9'. "Enviar
  IF sy-subrc <> 0.
    APPEND 'REFRESH' TO fcode.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZNFW010'
    ID 'ACTVT'  FIELD '04'. "danfe
  IF sy-subrc <> 0.
    APPEND 'DANFE' TO fcode.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZNFW010'
    ID 'ACTVT'  FIELD '31'. "confirmar
  IF sy-subrc <> 0.
    APPEND 'ATUALIZAR' TO fcode.
  ENDIF.


  SET PF-STATUS 'ST_0100'  EXCLUDING fcode.


  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR 'TL_0100'.

  PERFORM z_alv.
  PERFORM z_cria_alv.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'GERAR'.
      PERFORM z_gerar_nf.
    WHEN 'GERPED'.
      PERFORM z_cria_ped.
    WHEN 'REFRESH'.
      PERFORM z_envia_sefaz.
*-CS2021000723 - 18.10.2021 - JT - inicio
    WHEN 'PROCDOC'.
      PERFORM z_processar_documento.
    WHEN 'ARQXML' OR 'OPENXML' OR
         'ARQPDF' OR 'OPENPDF'.
      PERFORM z_gerar_arquivos_pdf_xml USING sy-ucomm.
    WHEN 'LOGAPROV'.
      PERFORM z_exibe_log_aprovacao.
    WHEN 'ESTRATEGIA'.
      PERFORM z_exibe_estrategia.
*-CS2021000723 - 18.10.2021 - JT - fim
    WHEN 'DANFE'.
      PERFORM z_gerar_danfe.
    WHEN 'ESTORNO'.
      PERFORM z_estornar.
    WHEN 'CONTRATO'.
      CALL TRANSACTION 'ZNFW0008'.
    WHEN 'ATUALIZAR'.
      PERFORM z_seleciona_dados.
      PERFORM z_tratar_dados.
  ENDCASE.

  wa_stable-row = abap_true.
  wa_stable-col = abap_true.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_dados .

  REFRESH: it_zfiwrt0021,
            it_zfiwrt0022,
            tl_0008,
            tl_zib_cont,
            tl_zib_chv,
            tl_zib_err,
            tl_0011,
            it_zfiwrt8,
            it_zfi0007,
            it_zfiwrt1000,
            it_kna1,
            it_t001,
            it_icon,
            it_estra_tot,
            it_saida.

*-CS2021000723 - 18.10.2021 - JT - inicio
  SELECT *
  FROM icon
  INTO TABLE it_icon
 WHERE name = 'ICON_RED_LIGHT'
    OR name = 'ICON_GREEN_LIGHT'
    OR name = 'ICON_YELLOW_LIGHT'
    OR name = 'ICON_LIGHT_OUT'.

  SORT it_icon BY name.
*-CS2021000723 - 18.10.2021 - JT - fim

  IF p_n_proc EQ abap_true.
    " Não iniciou faturamento
    SELECT *
      FROM zfiwrt0021
      INTO TABLE it_zfiwrt0021
     WHERE bukrs IN p_bukrs
      AND  ano   EQ p_ano
      AND  loekz NE 'X'
      AND  NOT EXISTS ( SELECT * FROM  zfiwrt0022 WHERE contrato EQ zfiwrt0021~contrato
                                                  AND   bukrs    EQ zfiwrt0021~bukrs
                                                  AND   branch   EQ zfiwrt0021~branch
                                                  AND   kunnr    EQ zfiwrt0021~kunnr
                                                  AND   ano      EQ p_ano
                                                  AND   mes      EQ p_mes ).
    " Gerou ZNFW mas não gerou doc fiscal
    SELECT *
          FROM zfiwrt0021
          INNER JOIN zfiwrt0022
            ON  zfiwrt0022~contrato EQ zfiwrt0021~contrato
            AND zfiwrt0022~bukrs    EQ zfiwrt0021~bukrs
            AND zfiwrt0022~branch   EQ zfiwrt0021~branch
            AND zfiwrt0022~kunnr    EQ zfiwrt0021~kunnr
            AND zfiwrt0022~ano      EQ p_ano
            AND zfiwrt0022~mes      EQ p_mes
          INNER JOIN zfiwrt0008
            ON   zfiwrt0008~seq_lcto EQ zfiwrt0022~seq_lcto
            AND  zfiwrt0008~docnum   EQ ' '
          APPENDING CORRESPONDING FIELDS OF TABLE it_zfiwrt0021
         WHERE zfiwrt0021~bukrs IN p_bukrs
          AND  zfiwrt0021~ano   EQ p_ano
          AND  zfiwrt0021~loekz NE 'X'.
    " gerou doc fiscal mas não autorizou
    SELECT *
          FROM zfiwrt0021
          INNER JOIN zfiwrt0022
            ON  zfiwrt0022~contrato EQ zfiwrt0021~contrato
            AND zfiwrt0022~bukrs    EQ zfiwrt0021~bukrs
            AND zfiwrt0022~branch   EQ zfiwrt0021~branch
            AND zfiwrt0022~kunnr    EQ zfiwrt0021~kunnr
            AND zfiwrt0022~ano      EQ p_ano
            AND zfiwrt0022~mes      EQ p_mes
          INNER JOIN zfiwrt0008
            ON   zfiwrt0008~seq_lcto = zfiwrt0022~seq_lcto
          INNER JOIN j_1bnfe_active
          ON j_1bnfe_active~docnum = zfiwrt0008~docnum
          APPENDING CORRESPONDING FIELDS OF TABLE it_zfiwrt0021
         WHERE zfiwrt0021~bukrs IN p_bukrs
          AND  zfiwrt0021~ano   EQ p_ano
          AND  zfiwrt0021~loekz NE 'X'
          AND ( j_1bnfe_active~docsta NE '1' OR j_1bnfe_active~cancel  EQ 'X' ).

    CHECK it_zfiwrt0021 IS NOT INITIAL.

    SELECT *
      FROM zfiwrt0022
      INTO CORRESPONDING FIELDS OF TABLE it_zfiwrt0022
      FOR ALL ENTRIES IN it_zfiwrt0021
      WHERE contrato EQ it_zfiwrt0021-contrato
        AND bukrs    EQ it_zfiwrt0021-bukrs
        AND branch   EQ it_zfiwrt0021-branch
        AND kunnr    EQ it_zfiwrt0021-kunnr
        AND ano      EQ it_zfiwrt0021-ano
        AND mes      EQ p_mes.

  ELSE. "Somente autorizadas SEFAZ
    IF p_data-high IS INITIAL.
      p_data-high = p_data-low.
    ENDIF.
    SELECT *
      FROM zfiwrt0022
       INNER JOIN zfiwrt0008
       ON   zfiwrt0008~seq_lcto = zfiwrt0022~seq_lcto
       INNER JOIN j_1bnfe_active
       ON j_1bnfe_active~docnum = zfiwrt0008~docnum
       AND j_1bnfe_active~docsta EQ '1'
       AND j_1bnfe_active~cancel NE 'X'
      INTO CORRESPONDING FIELDS OF TABLE it_zfiwrt0022
     WHERE zfiwrt0022~bukrs IN p_bukrs
      AND  zfiwrt0022~ano      EQ p_ano
      AND  zfiwrt0022~mes      EQ p_mes
      AND  zfiwrt0022~data_fatura BETWEEN p_data-low AND p_data-high.

    CHECK it_zfiwrt0022 IS NOT INITIAL.

    SELECT *
      FROM zfiwrt0021 INTO TABLE it_zfiwrt0021
      FOR ALL ENTRIES IN it_zfiwrt0022
      WHERE contrato EQ it_zfiwrt0022-contrato
        AND bukrs    EQ it_zfiwrt0022-bukrs
        AND branch   EQ it_zfiwrt0022-branch
        AND kunnr    EQ it_zfiwrt0022-kunnr
        AND ano      EQ it_zfiwrt0022-ano.

    CHECK it_zfiwrt0021 IS NOT INITIAL.
*
  ENDIF.

*-CS2021000723 - 18.10.2021 - JT - inicio
  IF it_zfiwrt0021[] IS NOT INITIAL.
    SELECT *
      FROM zfiwrt0007
      INTO TABLE it_zfi0007
       FOR ALL ENTRIES IN it_zfiwrt0021
     WHERE operacao EQ it_zfiwrt0021-operacao
       AND branch   EQ it_zfiwrt0021-branch.

    SORT it_zfi0007 BY usnam.
    DELETE ADJACENT DUPLICATES FROM it_zfi0007
                          COMPARING usnam.

*---busca estrategias
    LOOP AT it_zfi0007 INTO wa_zfi0007.

      FREE: it_lotes, it_estra, it_docs.

      CALL FUNCTION 'Z_NW_ESTRATEGIA_LISTA2'
        EXPORTING
          v_usuario      = wa_zfi0007-usnam
        IMPORTING
          msg            = l_msg
        TABLES
          t_zfiwrt0022   = it_zfiwrt0022
          t_lotes        = it_lotes
          t_estra        = it_estra
          t_docs         = it_docs
          t_docs_energia = it_docs_energia.

      APPEND LINES OF it_estra[] TO it_estra_tot[].
    ENDLOOP.

    SORT it_estra_tot BY bukrs lote.
    DELETE ADJACENT DUPLICATES FROM it_estra_tot
                          COMPARING bukrs lote.
  ENDIF.
*-CS2021000723 - 18.10.2021 - JT - fim

  IF it_zfiwrt0022[] IS NOT INITIAL.
    SELECT *
      FROM zfiwrt0008
      INTO TABLE tl_0008
      FOR ALL ENTRIES IN it_zfiwrt0022
       WHERE seq_lcto EQ it_zfiwrt0022-seq_lcto.


    IF tl_0008[] IS NOT INITIAL.
      SELECT *
        FROM zib_contabil
        INTO TABLE tl_zib_cont
         FOR ALL ENTRIES IN tl_0008
         WHERE obj_key EQ tl_0008-obj_key.

      SELECT *
        FROM zib_contabil_chv
        INTO TABLE tl_zib_chv
        FOR ALL ENTRIES IN tl_0008
         WHERE obj_key EQ tl_0008-obj_key.

      SELECT *
        FROM zib_contabil_err
        INTO TABLE tl_zib_err
        FOR ALL ENTRIES IN tl_0008
         WHERE obj_key EQ tl_0008-obj_key.

      SELECT *
            FROM zfiwrt0011
            INTO TABLE tl_0011
             FOR ALL ENTRIES IN tl_0008
             WHERE seq_lcto EQ tl_0008-seq_lcto
               AND dmbtr    GT 0.

      SELECT *
        FROM  zfiwrt0008 INTO TABLE it_zfiwrt8
       FOR ALL ENTRIES IN it_zfiwrt0022
       WHERE seq_lcto EQ it_zfiwrt0022-seq_lcto.
    ENDIF.

    IF it_zfiwrt8 IS NOT INITIAL.
      SELECT *
        FROM  zfiwrt1000 INTO TABLE it_zfiwrt1000
        FOR ALL ENTRIES IN it_zfiwrt8
       WHERE seq_lcto EQ it_zfiwrt8-seq_lcto.
    ENDIF.
  ENDIF.

  SELECT *
    FROM kna1 INTO TABLE it_kna1
   FOR ALL ENTRIES IN it_zfiwrt0021
    WHERE kunnr EQ it_zfiwrt0021-kunnr.

  SELECT *
    FROM t001 INTO TABLE it_t001
   FOR ALL ENTRIES IN it_zfiwrt0021
   WHERE bukrs EQ it_zfiwrt0021-bukrs.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_TRATAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_tratar_dados .
  SORT: it_zfiwrt0022 BY contrato bukrs branch kunnr ano,
        it_kna1 BY kunnr,
        it_t001 BY bukrs,
        it_zfiwrt8 BY seq_lcto,
        it_zfiwrt1000 BY seq_lcto.

  LOOP AT it_zfiwrt0021 INTO wa_zfiwrt0021.

    wa_saida-contrato    = wa_zfiwrt0021-contrato.
    CASE wa_zfiwrt0021-tipo.
      WHEN 'LP'.
        wa_saida-tipo_contrato = 'Longo Prazo'.
      WHEN 'CP'.
        wa_saida-tipo_contrato = 'Curto Prazo'.
    ENDCASE.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_zfiwrt0021-kunnr BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-kunnr   = wa_kna1-kunnr.
      wa_saida-name1   = wa_kna1-name1.
    ENDIF.

    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs =   wa_zfiwrt0021-bukrs BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-bukrs   = wa_t001-bukrs.
      wa_saida-butxt   = wa_t001-butxt.
    ENDIF.

    wa_saida-texto_nota   = wa_zfiwrt0021-texto_nota.
    wa_saida-operacao     = wa_zfiwrt0021-operacao.
    wa_saida-branch       = wa_zfiwrt0021-branch.
    wa_saida-matnr        = wa_zfiwrt0021-matnr.
    wa_saida-banco        = wa_zfiwrt0021-banco.
    wa_saida-agencia_banc = wa_zfiwrt0021-agencia_banc.
    wa_saida-conta_banc   = wa_zfiwrt0021-conta_banc.
    wa_saida-vencimento   = wa_zfiwrt0021-vencimento.
    wa_saida-vencimento_f = wa_zfiwrt0021-vencimento_f.

    READ TABLE it_zfiwrt0022 INTO wa_zfiwrt0022 WITH KEY contrato = wa_zfiwrt0021-contrato
                                                         bukrs    = wa_zfiwrt0021-bukrs
                                                         branch   = wa_zfiwrt0021-branch
                                                         kunnr    = wa_zfiwrt0021-kunnr
                                                         ano      = wa_zfiwrt0021-ano
                                                         BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-ebeln = wa_zfiwrt0022-ebeln_en.
      IF wa_zfiwrt0022-ebeln_en IS INITIAL.
        CONCATENATE  icon_warning wa_zfiwrt0022-ebeln_en INTO wa_saida-ebeln_flag SEPARATED BY ' - '.
      ELSE.
        SELECT COUNT(*)  "Deletado
          FROM ekpo
          WHERE ebeln EQ wa_zfiwrt0022-ebeln_en
          AND loekz   NE ' '.
        IF sy-subrc = 0.
          CLEAR: wa_zfiwrt0022-ebeln_en, wa_saida-ebeln.
          wa_zfiwrt0022-ebeln_en = icon_warning .
        ELSE.
          CONCATENATE  icon_checked wa_zfiwrt0022-ebeln_en INTO wa_saida-ebeln_flag SEPARATED BY ' - '.
          SELECT SINGLE * FROM zfiwrt0008 INTO @DATA(wa_t0008)
               WHERE seq_lcto EQ @wa_zfiwrt0022-seq_lcto.
          IF wa_t0008-docnum IS NOT INITIAL.
            SELECT SINGLE *
            FROM j_1bnfe_active
             INTO @DATA(wl_active)
              WHERE docnum EQ @wa_t0008-docnum.
            IF sy-subrc = 0.
              CONCATENATE wl_active-regio wl_active-nfyear wl_active-nfmonth wl_active-stcd1
                  wl_active-model wl_active-serie  wl_active-nfnum9  wl_active-docnum9 wl_active-cdv
                  INTO DATA(v_chave_doc).
              SELECT SINGLE *
              FROM zib_nfe_dist_ter
              INTO @DATA(w_nfe)
              WHERE chave_nfe EQ @v_chave_doc.
              "AND   mblnr     NE ' '.
              IF sy-subrc = 0 AND w_nfe-mblnr NE ''.
                CONCATENATE  icon_checked w_nfe-mblnr INTO wa_saida-migo_flag SEPARATED BY ' - '.
                wa_saida-se_recordid = w_nfe-se_recordid.
                IF w_nfe-ebeln IS INITIAL.
                  UPDATE zib_nfe_dist_ter SET ebeln  = wa_zfiwrt0022-ebeln_en
                  WHERE chave_nfe EQ v_chave_doc.
                  COMMIT WORK.
                ENDIF.
              ELSE.
                IF w_nfe-ebeln IS INITIAL.
                  UPDATE zib_nfe_dist_ter SET ebeln  = wa_zfiwrt0022-ebeln_en
                  WHERE chave_nfe EQ v_chave_doc.
                  COMMIT WORK.
                ENDIF.
                wa_saida-migo_flag = icon_generate.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
      wa_saida-seq_lcto = wa_zfiwrt0022-seq_lcto.
      READ TABLE it_zfiwrt8 INTO wa_zfiwrt8 WITH KEY seq_lcto = wa_zfiwrt0022-seq_lcto BINARY SEARCH.

      READ TABLE it_zfiwrt1000 INTO wa_zfiwrt1000 WITH KEY seq_lcto = wa_zfiwrt8-seq_lcto
                                                           field    = 'DOCNUM'.
      IF sy-subrc IS INITIAL.
        wa_saida-docnum_flag = icon_status_critical.
      ELSE.
        IF wa_zfiwrt8-docnum IS INITIAL.
          wa_saida-docnum_flag = icon_warning.
        ELSE.
          SHIFT wa_zfiwrt8-docnum LEFT DELETING LEADING '0'.
          CONCATENATE  icon_checked wa_zfiwrt8-docnum INTO wa_saida-docnum_flag SEPARATED BY ' - '.
          wa_saida-docnum =  |{ wa_zfiwrt8-docnum ALPHA = IN  }|.
        ENDIF.
      ENDIF.

      READ TABLE it_zfiwrt1000 INTO wa_zfiwrt1000 WITH KEY seq_lcto = wa_zfiwrt8-seq_lcto
                                                           field    = 'SEQ_LCTO'.
      IF sy-subrc IS INITIAL.
        wa_saida-seq_lcto_flag = icon_status_critical.
      ELSE.
        IF wa_zfiwrt8-seq_lcto IS INITIAL.
          wa_saida-seq_lcto_flag = icon_warning.
        ELSE.
          SHIFT wa_zfiwrt8-seq_lcto LEFT DELETING LEADING '0'.
          CONCATENATE  icon_checked wa_zfiwrt8-seq_lcto INTO wa_saida-seq_lcto_flag SEPARATED BY ' - '.
          wa_saida-seq_lcto =  |{ wa_zfiwrt8-seq_lcto ALPHA = IN  }|.
          IF wa_zfiwrt8-docs_estornados IS INITIAL.
            wa_saida-operacao = wa_zfiwrt8-operacao.
          ENDIF.
        ENDIF.
      ENDIF.

      wa_saida-nfenum        = wa_zfiwrt8-nfenum.

    ENDIF.

    CASE p_mes.
      WHEN '01'.
        wa_saida-tarifa        = wa_zfiwrt0021-tarifa01.
        wa_saida-montante      = wa_zfiwrt0021-montante01.
        wa_saida-mes           = '01'.
        wa_saida-ano           = wa_zfiwrt0021-ano.
      WHEN '02'.
        wa_saida-tarifa        = wa_zfiwrt0021-tarifa02.
        wa_saida-montante      = wa_zfiwrt0021-montante02.
        wa_saida-mes           = '02'.
        wa_saida-ano           = wa_zfiwrt0021-ano.
      WHEN '03'.
        wa_saida-tarifa        = wa_zfiwrt0021-tarifa03.
        wa_saida-montante      = wa_zfiwrt0021-montante03.
        wa_saida-mes           = '03'.
        wa_saida-ano           =  wa_zfiwrt0021-ano.
      WHEN '04'.
        wa_saida-tarifa        = wa_zfiwrt0021-tarifa04.
        wa_saida-montante      = wa_zfiwrt0021-montante04.
        wa_saida-mes           = '04'.
        wa_saida-ano           =  wa_zfiwrt0021-ano.
      WHEN '05'.
        wa_saida-tarifa        = wa_zfiwrt0021-tarifa05.
        wa_saida-montante      = wa_zfiwrt0021-montante05.
        wa_saida-mes           = '05'.
        wa_saida-ano           =  wa_zfiwrt0021-ano.
      WHEN '06'.
        wa_saida-tarifa        = wa_zfiwrt0021-tarifa06.
        wa_saida-montante      = wa_zfiwrt0021-montante06.
        wa_saida-mes           = '06'.
        wa_saida-ano           =  wa_zfiwrt0021-ano.
      WHEN '07'.
        wa_saida-tarifa        = wa_zfiwrt0021-tarifa07.
        wa_saida-montante      = wa_zfiwrt0021-montante07.
        wa_saida-mes           = '07'.
        wa_saida-ano           =  wa_zfiwrt0021-ano.
      WHEN '08'.
        wa_saida-tarifa        = wa_zfiwrt0021-tarifa08.
        wa_saida-montante      = wa_zfiwrt0021-montante08.
        wa_saida-mes           = '08'.
        wa_saida-ano           =  wa_zfiwrt0021-ano.
      WHEN '09'.
        wa_saida-tarifa        = wa_zfiwrt0021-tarifa09.
        wa_saida-montante      = wa_zfiwrt0021-montante09.
        wa_saida-mes           = '09'.
        wa_saida-ano           =  wa_zfiwrt0021-ano.
      WHEN '10'.
        wa_saida-tarifa        = wa_zfiwrt0021-tarifa10.
        wa_saida-montante      = wa_zfiwrt0021-montante10.
        wa_saida-mes           = '10'.
        wa_saida-ano           =  wa_zfiwrt0021-ano.
      WHEN '11'.
        wa_saida-tarifa        = wa_zfiwrt0021-tarifa11.
        wa_saida-montante      = wa_zfiwrt0021-montante11.
        wa_saida-mes           = '11'.
        wa_saida-ano           =  wa_zfiwrt0021-ano.
      WHEN '12'.
        wa_saida-tarifa        = wa_zfiwrt0021-tarifa12.
        wa_saida-montante      = wa_zfiwrt0021-montante12.
        wa_saida-mes           = '12'.
        wa_saida-ano           =  wa_zfiwrt0021-ano.
    ENDCASE.

    wa_saida-vlr_faturado = ( wa_saida-tarifa *  wa_saida-montante ).

    wa_saida-pgto_mes_seguinte = wa_zfiwrt0021-pgto_mes_seguinte.
    wa_saida-fatura_atu = wa_zfiwrt0021-fatura_atu.
    wa_saida-fatura_f = wa_zfiwrt0021-fatura_f.
    wa_saida-fatura_u = wa_zfiwrt0021-fatura_u.

*-CS2021000723 - 18.10.2021 - JT - inicio
*----------------------------------
*---define status aprovador
*----------------------------------
    CLEAR wa_estra_tot.
    READ TABLE it_estra_tot INTO wa_estra_tot WITH KEY bukrs = wa_saida-bukrs
                                                       lote  = wa_saida-seq_lcto
                                              BINARY SEARCH.
    IF     wa_estra_tot-estado = icon_checked."    AND
*          wa_estra_tot-opcoes = icon_system_undo.
      l_icon_name = 'ICON_GREEN_LIGHT'.
    ELSEIF wa_estra_tot-estado = icon_led_yellow. " AND
*          wa_estra_tot-opcoes = ''.
      l_icon_name = 'ICON_YELLOW_LIGHT'.
    ELSEIF wa_estra_tot-estado = icon_reject."     AND
*          wa_estra_tot-opcoes = ''.
      l_icon_name = 'ICON_RED_LIGHT'.
    ELSE.
      l_icon_name = 'ICON_LIGHT_OUT'.
    ENDIF.

    READ TABLE it_icon INTO wa_icon WITH KEY name = l_icon_name.
    wa_saida-status = wa_icon-id.
*-CS2021000723 - 18.10.2021 - JT - fim

    IF wa_saida-vlr_faturado GT 0.
      APPEND wa_saida TO it_saida.
    ENDIF.

    CLEAR: wa_saida, wa_kna1, wa_t001, wa_zfiwrt0022, wa_zfiwrt0021, wa_zfiwrt8.
  ENDLOOP.

  IF r_ped = 'X'.
    DELETE it_saida WHERE  ebeln IS NOT INITIAL.
  ENDIF.


  PERFORM z_busca_dados_atualizado.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_alv .

  CLEAR wl_fcat.
  REFRESH it_fcat[].

  PERFORM preenche_cat USING:
*-CS2021000723 - 18.10.2021 - JT - inicio
          'STATUS'           'Stat.Aprov'         '11'       ''     ' '   ''    ''    '',
*-CS2021000723 - 18.10.2021 - JT - fim
          'CONTRATO'         'Contrato'           '10'       ''     'X'   ''    ''    '',
          'ANO'              'Ano'                '05'       ''     ''    ''    ''    '',
          'TIPO_CONTRATO'    'Tipo'               '15'       ''     ''    ''    ''    '',
          'KUNNR'            'Cód.Cliente'        '10'       ''     ''    ''    ''    '',
          'NAME1'            'Descrição'          '30'       ''     ''    ''    ''    '',
          'BUKRS'            'Cód.Empresa'        '10'       ''     ''    ''    ''    '',
          'BUTXT'            'Descrição'          '25'       ''     ''    ''    ''    '',
          'TEXTO_NOTA'       'Texto Nota'         '30'       ''     ''    ''    ''    '',
          'OPERACAO'         'Operação'           '08'       ''     ''    ''    ''    '',
          'TARIFA'           'Tarifa'             '10'       ''     ''    ''    ''    '',
          'MONTANTE'         'Montante'           '13'       ''     ''    ''    ''    '',
          'VLR_FATURADO'     'Valor Faturado'     '13'       ''     ''    ''    ''    '',
          'SEQ_LCTO_FLAG'    'Seq.Lancto'         '10'       ''     'X'   ''    ''    '',
          'DOCNUM_FLAG'      'Doc.Num'            '20'       ''     'X'   ''    ''    '',
          'DOC_CONTAB_FLAG'  'Doc.Contabil'       '15'       ''     'X'   ''    ''    '',
          'NFNUM_FLAG'       'Num. NFE'           '10'       ''     'X'   ''    ''    '',
          'EBELN_FLAG'       'Pedido'             '10'       ''     'X'   ''    ''    '',
          'MIGO_FLAG'        'Migo'               '10'       ''     'X'   ''    ''    '',
          'SE_RECORDID'      'SM Miro'            '10'       ''     ' '   ''    ''    '',
          'FATURA_ATU'       'Fat.Mês Atual'      '20'       ''     'X'   ''    ''    '',
          'FATURA_F'         'Dia Fixo Fat.'      '13'       ''     'X'   ''    ''    '',
          'FATURA_U'         'Dia Util Fat.'      '13'       ''     'X'   ''    ''    '',
          'MESSAGE'          'Mensagem SEFAZ'     '60'       ''     ' '   ''    ''    ''.

ENDFORM.


FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_hot)
                        VALUE(p_sum)
                        VALUE(p_just)
                        VALUE(p_cor).

  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-outputlen = p_tam.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum    = p_sum.
  wl_fcat-just      = p_just.
  wl_fcat-emphasize = p_cor.
  IF p_campo  = 'OPERACAO'.
    wl_fcat-edit = 'X'.
  ENDIF.

  APPEND wl_fcat TO  it_fcat.
  CLEAR wl_fcat.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_CRIA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_cria_alv .
  wa_layout-ctab_fname = 'COLOR'.
  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF g_grid IS INITIAL AND  g_custom_container IS NOT INITIAL.
      CREATE OBJECT g_grid
        EXPORTING
          i_parent          = g_custom_container
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.


    SET HANDLER lcl_event_handler=>on_hotsopt_click  FOR g_grid.
    SET HANDLER lcl_event_handler=>on_data_changed  FOR g_grid.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
      CHANGING
        it_outtab                     = it_saida[]
        it_fieldcatalog               = it_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


*    CREATE OBJECT OB_TIMER
*      EXPORTING
*        PARENT = CONTAINER_1.
*
*    CREATE OBJECT OB_RECEV.
*
*    SET HANDLER OB_RECEV->HANDLE_FINISHED FOR OB_TIMER.
*
*    OB_TIMER->INTERVAL = 2.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.
*    CALL METHOD G_GRID->SET_FRONTEND_FIELDCATALOG
*      EXPORTING
*        IT_FIELDCATALOG = IT_FCAT.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSAR_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_exibe_log_aprovacao.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  DESCRIBE TABLE tg_selectedrow[] LINES DATA(l_lines).

  IF tg_selectedrow[] IS INITIAL.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'E'.
    EXIT.
  ENDIF.

  IF l_lines > 1.
    MESSAGE 'Selecione somente uma linha!' TYPE 'E'.
    EXIT.
  ENDIF.

  READ TABLE tg_selectedrow INTO wg_selectedrow INDEX 1.
  READ TABLE it_saida       INTO wa_saida       INDEX wg_selectedrow-index.

  CALL FUNCTION 'Z_EXIBE_LOG_APROVACAO'
    EXPORTING
      i_seq_lcto = wa_saida-seq_lcto.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSAR_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_exibe_estrategia.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  DESCRIBE TABLE tg_selectedrow[] LINES DATA(l_lines).

  IF tg_selectedrow[] IS INITIAL.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'E'.
    EXIT.
  ENDIF.

  IF l_lines > 1.
    MESSAGE 'Selecione somente uma linha!' TYPE 'E'.
    EXIT.
  ENDIF.

  READ TABLE tg_selectedrow INTO wg_selectedrow INDEX 1.
  READ TABLE it_saida       INTO wa_saida       INDEX wg_selectedrow-index.

  CALL FUNCTION 'Z_EXIBE_ESTRATEGIA'
    EXPORTING
      i_operacao = wa_saida-operacao.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSAR_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_gerar_arquivos_pdf_xml USING p_ucomm.

  DATA: t_docs TYPE TABLE OF zsds0041,
        w_docs TYPE zsds0041,
        l_type TYPE z_type_file.

  l_type = p_ucomm.

  FREE: t_docs.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  IF tg_selectedrow[] IS INITIAL.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'E'.
    EXIT.
  ENDIF.

  LOOP AT tg_selectedrow INTO wg_selectedrow.
    CLEAR wa_saida.
    READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.

    w_docs-docnum  = wa_saida-docnum.
    APPEND w_docs TO t_docs.
  ENDLOOP.

*------------------------------
*-Executa processo
*------------------------------
  CALL FUNCTION 'Z_DOWNLOAD_XML_PDF'
    EXPORTING
      i_type_file     = l_type
    TABLES
      t_documentos    = t_docs
    EXCEPTIONS
      error_open_file = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'Houve erro ao gerar o Arquivo!' DISPLAY LIKE 'E'.
  ELSE.
    MESSAGE s024(sd) WITH 'Processo finalizado.'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSAR_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_processar_documento.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  DESCRIBE TABLE tg_selectedrow[] LINES DATA(l_lines).

  IF tg_selectedrow[] IS INITIAL.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'E'.
    EXIT.
  ENDIF.

  IF l_lines > 1.
    MESSAGE 'Selecione somente uma linha!' TYPE 'E'.
    EXIT.
  ENDIF.

  LOOP AT tg_selectedrow INTO wg_selectedrow.

    CLEAR wa_saida.
    READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.

    IF wa_saida-status = icon_red_light.
      MESSAGE w024(sd) WITH |Documento não pode |
                            |ser processado!.|.
      EXIT.
    ENDIF.

    SELECT operacao
      FROM zfiwrt0007
      INTO @DATA(wa_0007)
        UP TO 1 ROWS
     WHERE operacao EQ @wa_saida-operacao
       AND branch   EQ @wa_saida-branch.
    ENDSELECT.

    IF sy-subrc = 0.
      IF wa_saida-status = icon_yellow_light.
        MESSAGE w024(sd) WITH |Documentos que estão aguardando aprovação, |
                              |não serão processados!.|.
        EXIT.
      ELSE.
        MESSAGE w024(sd) WITH |Lançamento não necessita de processamento manual.|.
*                           |Favor realizar o processo pelo botão "Gerar Nota Writer".|.
        EXIT.
      ENDIF.
    ENDIF.

    IF wa_saida-status = icon_green_light AND
       wa_saida-seq_lcto IS INITIAL.
      MESSAGE w024(sd) WITH |Documento não possue Seq.Lancamento |
                            |gerada!|.
      EXIT.
    ENDIF.

    CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
      EXPORTING
        i_seq_lcto = wa_saida-seq_lcto.

    MESSAGE s024(sd) WITH |Lançamento Processado!|.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_GERAR_NF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_gerar_nf .

  DATA: v_zfbdt      TYPE zfiwrt0011-zfbdt,
        v_data_aux   TYPE sy-datum,
        wl_cont      TYPE sy-tabix,
        wl_linha     TYPE sy-tabix,
        wl_cont_aux  TYPE sy-tabix,
        wl_cont_aux2 TYPE sy-tabix,
        v_matnr18    TYPE matnr18.

  DATA: l_data   TYPE  p0001-begda,
        l_days   TYPE  t5a4a-dlydy,
        l_months TYPE  t5a4a-dlymo,
        l_signum TYPE  t5a4a-split,
        l_years  TYPE  t5a4a-dlyyr.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  IF tg_selectedrow[] IS INITIAL.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'E'.
    EXIT.
  ELSE.

    SELECT *  FROM  zfiwrt0001  INTO TABLE it_zfiwrt0001
      FOR ALL ENTRIES IN it_saida
    WHERE operacao EQ it_saida-operacao.

    IF sy-subrc <> 0.
      MESSAGE 'Não existe Operação Nota Writer Cadastado' TYPE 'I'.
      EXIT.
    ENDIF.

    SELECT  * FROM t001w INTO TABLE it_t001w
      FOR ALL ENTRIES IN it_saida
      WHERE werks EQ it_saida-branch.

    SELECT  * FROM j_1baa INTO TABLE  it_j_1baa
     FOR ALL ENTRIES IN it_zfiwrt0001
      WHERE nftype EQ it_zfiwrt0001-nftype.

    SELECT  *  FROM kna1 INTO TABLE it_kna1
      FOR ALL ENTRIES IN it_saida
      WHERE kunnr EQ it_saida-kunnr.

    SELECT *  FROM zfiwrt0002  INTO TABLE it_zfiwrt0002
      FOR ALL ENTRIES IN it_saida
      WHERE operacao EQ it_saida-operacao.

    IF sy-subrc IS INITIAL.

      SELECT * FROM j_1baj  INTO TABLE it_j_1baj
        FOR ALL ENTRIES IN it_zfiwrt0002
        WHERE taxtyp EQ it_zfiwrt0002-taxtyp.

      SELECT *  FROM j_1bajt INTO TABLE it_j_1bajt
        FOR ALL ENTRIES IN it_zfiwrt0002
        WHERE spras  EQ sy-langu
        AND   taxtyp EQ it_zfiwrt0002-taxtyp.

    ENDIF.

    SELECT *  FROM zfiwrt0003  INTO TABLE it_zfiwrt0003
      FOR ALL ENTRIES IN it_zfiwrt0021
       WHERE operacao EQ it_zfiwrt0021-operacao.

    IF sy-subrc IS INITIAL.

      SELECT  *  FROM tbsl  INTO TABLE it_tbsl
        FOR ALL ENTRIES IN it_zfiwrt0003
         WHERE bschl EQ it_zfiwrt0003-bschl.

      SELECT  *  FROM skat INTO TABLE it_skat
        FOR ALL ENTRIES IN it_zfiwrt0003
          WHERE spras EQ sy-langu
            AND ktopl EQ '0050'
            AND saknr EQ it_zfiwrt0003-hkont.

    ENDIF.

    SELECT  *  FROM zfiwrt0004 INTO TABLE  it_zfiwrt0004
      FOR ALL ENTRIES IN it_saida
       WHERE operacao EQ it_saida-operacao.

    SELECT  * FROM zfiwrt0005  INTO TABLE it_zfiwrt0005
      FOR ALL ENTRIES IN it_saida
     WHERE operacao EQ it_saida-operacao.


    SELECT * FROM zfiwrt0006  INTO TABLE it_zfiwrt0006
      FOR ALL ENTRIES IN it_saida
     WHERE operacao EQ it_saida-operacao.

    SELECT  * FROM zfiwrt0007   INTO TABLE it_zfiwrt0007
      FOR ALL ENTRIES IN it_saida
        WHERE operacao EQ it_saida-operacao
          AND branch   EQ it_saida-branch
          AND tipo     EQ 'W'.

    SORT: it_zfiwrt0004 BY operacao,
          it_zfiwrt0005 BY operacao seqnum linnum,
          it_zfiwrt0006 BY operacao indcoper,
          it_zfiwrt0007 BY operacao.

*-CS2021000723 - 18.10.2021 - JT - inicio
    LOOP AT tg_selectedrow INTO wg_selectedrow.

      FREE:    it_zfiwrt0008,
               wa_zfiwrt0008,
               it_zfiwrt0009,
               wa_zfiwrt0009,
               it_zfiwrt0022,
               wa_zfiwrt0022,
               it_zfiwrt0010,
               it_zfiwrt0011,
               it_zfiwrt0012,
               it_zfiwrt0013,
               it_zfiwrt0015,
               it_zfiwrt22,
               tg_mensagens,
               wa_saida,      wa_saida_aux,  wa_zfiwrt0001, wa_zfiwrt0002,
               wa_zfiwrt0003, wa_zfiwrt0004, wa_zfiwrt0005, wa_zfiwrt0006,
               wa_zfiwrt0007, wa_zfiwrt0008, wa_zfiwrt0009, wa_zfiwrt0010,
               wa_zfiwrt0011, wa_zfiwrt0012, wa_zfiwrt0013, wa_zfiwrt0015,
               wa_zfiwrt22,   wa_t001w,      wa_j_1baa,     wa_kna1,
               wa_j_1baj,     wa_j_1bajt,    wa_tbsl,       wa_skat,
               wa_cskb,       wa_marc,       wa_mara.

      READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.
      "
      IF ( wa_saida-seq_lcto IS INITIAL AND wa_saida-docnum IS INITIAL )
          OR wa_saida-docs_estornados = 'X'
          OR wa_saida-docnum_flag EQ icon_status_critical..

        REFRESH tl_docest.
        MOVE: wa_saida-seq_lcto TO tl_docest-seq_lcto.
        APPEND tl_docest.
        CLEAR: tl_docest.
        "
        CALL FUNCTION 'ZNFW_ESTORNA_SEQ_LCTO'
          TABLES
            t_docs = tl_docest.

        CLEAR tl_docest.
        READ TABLE tl_docest
           WITH KEY seq_lcto = wa_saida-seq_lcto
               BINARY SEARCH.

        sy-subrc = 4.
        IF wa_saida-seq_lcto  IS NOT INITIAL.
          SELECT SINGLE *
            FROM zfiwrt0008
            INTO @DATA(w008)
            WHERE seq_lcto = @wa_saida-seq_lcto.
          SELECT SINGLE *
            FROM zib_contabil_chv
            INTO @DATA(wchv)
            WHERE obj_key = @w008-obj_key.
        ENDIF.

        IF ( tl_docest-belnr_est IS INITIAL AND sy-subrc = 0 )
            AND wa_saida-docs_estornados = 'X'
            AND wa_saida-docnum_flag NE icon_status_critical.
          CONTINUE.
        ENDIF.

        MOVE-CORRESPONDING wa_saida TO wa_saida_aux.

        CLEAR: wl_cont_aux2, wl_cont_aux, wl_cont.
        REFRESH: tg_mensagens.
        LOOP AT it_zfiwrt0005 INTO wa_zfiwrt0005 WHERE  operacao = wa_saida-operacao.
          tg_mensagens-linnum    =  wa_zfiwrt0005-linnum.
          tg_mensagens-seqnum    =  wa_zfiwrt0005-seqnum.
          tg_mensagens-message   =  wa_zfiwrt0005-message.
          APPEND tg_mensagens.
          ADD 1 TO wl_cont.
        ENDLOOP.

        REFRESH: tg_movest.
        LOOP AT it_zfiwrt0004 INTO wa_zfiwrt0004 WHERE  operacao = wa_saida-operacao.
          MOVE: wa_zfiwrt0004-bwart   TO tg_movest-bwart,
                wa_zfiwrt0004-tcode   TO tg_movest-tcode,
                wa_zfiwrt0004-mwskz1  TO tg_movest-mwskz1,
                wa_zfiwrt0004-estorno TO tg_movest-estorno.
          APPEND tg_movest.
          CLEAR: tg_movest, wa_zfiwrt0004.
        ENDLOOP.

        READ TABLE it_zfiwrt0001 INTO wa_zfiwrt0001 WITH KEY  operacao = wa_saida-operacao.

        READ TABLE it_j_1baa INTO wa_j_1baa WITH KEY  nftype =  wa_zfiwrt0001-nftype.

        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_saida-kunnr.

        READ TABLE it_t001w INTO wa_t001w  WITH KEY werks = wa_saida-branch.

        wl_indcoper = 'D'.
        PERFORM f_define_origem_destino USING    wa_kna1
                                                 wa_lfa1
                                                 wa_t001w
                                                 wa_j_1baa
                                                 wa_zfiwrt0001
                                        CHANGING wl_indcoper
                                                 wl_texto_fiscal.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_saida-matnr
          IMPORTING
            output = v_matnr18.

        wa_saida-matnr = v_matnr18.

        SELECT SINGLE * FROM mara
          INTO wa_mara
          WHERE matnr EQ wa_saida-matnr.

        READ TABLE it_zfiwrt0006 INTO wa_zfiwrt0006 WITH KEY  operacao = wa_saida-operacao
                                                              indcoper = wl_indcoper BINARY SEARCH.

        IF sy-subrc NE 0.
          MESSAGE |Erro na operacao { wa_saida-operacao } | TYPE 'I'.
          CONTINUE.
        ENDIF.

*-------------------------------------------------------
*----- cabecalho
*-------------------------------------------------------
        MOVE:  sy-mandt                         TO wa_zfiwrt0008-mandt,
               wa_saida-operacao                TO wa_zfiwrt0008-operacao,
               wa_saida-bukrs                   TO wa_zfiwrt0008-bukrs,
               wa_saida-branch                  TO wa_zfiwrt0008-branch,
               wa_zfiwrt0001-parvw              TO wa_zfiwrt0008-parvw,
               wa_saida-kunnr                   TO wa_zfiwrt0008-parid,
               wa_zfiwrt0001-nftype             TO wa_zfiwrt0008-nftype,
               wa_saida-branch                  TO wa_zfiwrt0008-move_plant,
               wa_zfiwrt0001-energia            TO wa_zfiwrt0008-energia,
               wa_zfiwrt0001-servico            TO wa_zfiwrt0008-servico,
               wa_zfiwrt0001-complemento        TO wa_zfiwrt0008-complemento,
               'CIF'                            TO wa_zfiwrt0008-inco1,
               'CIF'                            TO wa_zfiwrt0008-inco2,
               wa_zfiwrt0001-referencia         TO wa_zfiwrt0008-referencia,
               wa_zfiwrt0006-cfop               TO wa_zfiwrt0008-cfop,
               wa_zfiwrt0006-taxlw1             TO wa_zfiwrt0008-taxlw1,
               wa_zfiwrt0006-taxlw2             TO wa_zfiwrt0008-taxlw2,
               wa_zfiwrt0006-taxlw4             TO wa_zfiwrt0008-taxlw4,
               wa_zfiwrt0006-taxlw5             TO wa_zfiwrt0008-taxlw5,
               wa_zfiwrt0006-opertyp            TO wa_zfiwrt0008-opertyp,
               wa_zfiwrt0006-taxcode            TO wa_zfiwrt0008-taxcode,
*              'A'                              TO wa_zfiwrt0008-status,
               ' '                              TO wa_zfiwrt0008-status,
               sy-uname                         TO wa_zfiwrt0008-usuario_ult_mod,
               sy-datum                         TO wa_zfiwrt0008-dt_ult_mod,
               sy-uzeit                         TO wa_zfiwrt0008-hr_ult_mod,
               p_data-low                       TO wa_zfiwrt0008-budat,
               p_data-low                       TO wa_zfiwrt0008-bldat,
               sy-uname                         TO wa_zfiwrt0008-usnam,
               sy-datum                         TO wa_zfiwrt0008-dt_criacao,
               sy-uzeit                         TO wa_zfiwrt0008-hr_criacao.

*-------------------------------------------------------
*-------Faturamento de Contratos de Energia
*-------------------------------------------------------
        MOVE: sy-mandt                   TO wa_zfiwrt22-mandt,
              wa_saida-contrato          TO wa_zfiwrt22-contrato,
              wa_saida-bukrs             TO wa_zfiwrt22-bukrs,
              wa_saida-branch            TO wa_zfiwrt22-branch,
              wa_saida-kunnr             TO wa_zfiwrt22-kunnr,
              wa_saida-ano               TO wa_zfiwrt22-ano,
              wa_saida-mes               TO wa_zfiwrt22-mes,
              wa_saida-montante          TO wa_zfiwrt22-montante,
              wa_saida-tarifa            TO wa_zfiwrt22-tarifa,
              p_data-low                 TO wa_zfiwrt22-data_fatura,
              sy-uname                   TO wa_zfiwrt22-usuario,
              sy-datum                   TO wa_zfiwrt22-dt_modf,
              sy-uzeit                   TO wa_zfiwrt22-hr_modf.


        SELECT SINGLE *  FROM marc  INTO wa_marc
          WHERE matnr EQ  wa_saida-matnr.

        REFRESH  tg_itens.
        tg_itens-itmnum  = 10.
        tg_itens-matnr   = wa_saida-matnr.
        SELECT SINGLE maktx FROM makt INTO tg_itens-maktx WHERE matnr EQ wa_saida-matnr.
        tg_itens-cfop    = wa_zfiwrt0006-cfop.
        tg_itens-werks   = wa_saida-branch.
        tg_itens-menge   = wa_saida-montante.
        tg_itens-meins   = wa_mara-meins.
        tg_itens-netpr   = wa_saida-tarifa.
        tg_itens-netwr   = ( tg_itens-netpr  * tg_itens-menge ).
        tg_itens-steuc   = wa_marc-steuc.
        APPEND tg_itens.

*-------------------------------------------------------
*------ itens
*-------------------------------------------------------
        READ TABLE tg_itens INDEX 1.
        MOVE: sy-mandt          TO wa_zfiwrt0009-mandt,
         tg_itens-itmnum        TO wa_zfiwrt0009-itmnum,
         tg_itens-matnr         TO wa_zfiwrt0009-matnr,
         tg_itens-cfop          TO wa_zfiwrt0009-cfop,
         tg_itens-charg         TO wa_zfiwrt0009-charg,
         tg_itens-menge         TO wa_zfiwrt0009-menge,
         tg_itens-meins         TO wa_zfiwrt0009-meins,
         tg_itens-netpr         TO wa_zfiwrt0009-netpr,
         tg_itens-netwr         TO wa_zfiwrt0009-netwr,
         wa_zfiwrt0001-itmtyp   TO wa_zfiwrt0009-itmtyp,
         tg_itens-werks         TO wa_zfiwrt0009-bwkey,
         tg_itens-lgort         TO wa_zfiwrt0009-lgort,
         tg_itens-anln1         TO wa_zfiwrt0009-anln1,
         tg_itens-anln2         TO wa_zfiwrt0009-anln2.

*-------------------------------------------------------
*------ mensagens e contabil
*-------------------------------------------------------
        PERFORM f_monta_mensagem CHANGING v_zfbdt.
        PERFORM f_prepara_contabil.

        wa_zfiwrt0008-zfbdt = v_zfbdt.
        wa_zfiwrt0008-zlsch = 'U'.

*-------------------------------------------------------
*------ parceiro
*-------------------------------------------------------
        REFRESH tg_parc.
        tg_parc-parvw = wa_zfiwrt0008-parvw.
        tg_parc-parid = wa_zfiwrt0008-parid.
        APPEND tg_parc.

        READ TABLE tg_parc INDEX 1.
        MOVE: sy-mandt                 TO wa_zfiwrt0015-mandt,
              tg_parc-parvw            TO wa_zfiwrt0015-parvw,
              tg_parc-parid            TO wa_zfiwrt0015-parid.


        " a classe vai calcular o imposto / contabil
        REFRESH: it_zfiwrt0011, it_zfiwrt0010.
        " a classe vai calcular o imposto / contabil


*--------------------------------------------------------------------------------------------------------*
* ------montar tabelas / gravar documento
*--------------------------------------------------------------------------------------------------------*
        TRY.
            zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                                       )->set_cabecalho(       EXPORTING i_cabecalho   = wa_zfiwrt0008
                                                       )->set_fatura_energia(  EXPORTING i_fatura      = wa_zfiwrt22
                                                       )->add_item(            EXPORTING i_item        = wa_zfiwrt0009
                                                       )->validar_registro(
                                                       )->prepara_lancamento(  EXPORTING i_impostos    = it_zfiwrt0010[]
                                                                                         i_contabil    = it_zfiwrt0011[]
                                                       )->add_parceiro(        EXPORTING i_parceiro    = wa_zfiwrt0015
                                                       )->set_monta_mensagens( EXPORTING i_mensagens   = it_zfiwrt0005[]
                                                       )->gravar_documento(    EXPORTING i_nao_valida  = abap_true
                                                                                         i_nao_prepara = abap_true
                                                                               IMPORTING e_seq_lcto    = DATA(_seq_lcto_gerado)
                                                       ).

            COMMIT WORK AND WAIT.

            IF _seq_lcto_gerado IS NOT INITIAL.
              MESSAGE |Lançamento { _seq_lcto_gerado } gerado com sucesso!| TYPE 'S'.

              IF wa_saida-status = icon_green_light.
                CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
                  EXPORTING
                    i_seq_lcto = _seq_lcto_gerado.
              ENDIF.
            ELSE.
              MESSAGE |Houve um erro ao gravar o lançamento!| TYPE 'S'.
            ENDIF.

          CATCH zcx_nf_writer INTO DATA(zcx_nf_writer).
            zcx_nf_writer->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        ENDTRY.

        APPEND wa_saida_aux TO it_saida_aux.
      ELSE.
        MESSAGE 'Linha selecionada ja processada!' TYPE 'E'.
        EXIT.
      ENDIF.
    ENDLOOP.

    PERFORM z_busca_dados_atualizado.
  ENDIF.
*-CS2021000723 - 18.10.2021 - JT - fim

ENDFORM.

*CS2021000785 Automatizar Pedidos de Compra/Lançtos NFs
FORM z_cria_ped.
  "ALRS -
  DATA:
    it_editor_ped             TYPE STANDARD TABLE OF ty_editor,  "Tabela para extração do texto da solicitação de compra
    wa_editor_ped             TYPE ty_editor,
    it_return_ped             TYPE STANDARD TABLE OF bapiret2, "TABLE OF BAPIRET2 WITH HEADER LINE,
    wa_return_ped             TYPE bapiret2,
    it_poitem_ped             TYPE STANDARD TABLE OF bapimepoitem, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
    wa_poitem_ped             TYPE bapimepoitem,
    it_poitemx_ped            TYPE STANDARD TABLE OF bapimepoitemx,
    wa_poitemx_ped            TYPE bapimepoitemx,
    it_poaccount              TYPE STANDARD TABLE OF bapimepoaccount,
    wa_poaccount              TYPE bapimepoaccount,
    it_poaccountx             TYPE STANDARD TABLE OF bapimepoaccountx,
    wa_poaccountx             TYPE bapimepoaccountx,
    wa_poheader_ped           TYPE bapimepoheader,
    wa_poheaderx_ped          TYPE bapimepoheaderx,
    it_popartner_ped          TYPE STANDARD TABLE OF bapiekkop, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
    wa_popartner_ped          TYPE bapiekkop, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
    it_bapimepotextheader_ped TYPE STANDARD TABLE OF bapimepotextheader,
    wa_bapimepotextheader_ped TYPE bapimepotextheader,
    it_pocond                 TYPE STANDARD TABLE OF bapimepocond,
    wa_pocond                 TYPE bapimepocond,
    it_pocondx                TYPE STANDARD TABLE OF bapimepocondx,
    wa_pocondx                TYPE bapimepocondx,
    purchaseorder             LIKE bapimepoheader-po_number,
    v_ekorg                   TYPE t024w-ekorg,
    v_branch                  TYPE t001w-j_1bbranch,
    v_bukrs                   TYPE j_1bbranch-bukrs,
    v_lifnr                   TYPE lfa1-lifnr,
    v_ebeln                   TYPE ekko-ebeln,
    vnetpr_final              TYPE zmmt0037-netpr,
    w_zmmt0035                TYPE zmmt0035,
    w_answer(1),
    v_altera_pgt(1),
    v_altera(1),
    vebeln_flag(20).

   DATA: VG_MATNR18 TYPE CHAR18.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  IF tg_selectedrow[] IS INITIAL.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'E'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = 'Deseja criar pedido ?'
      text_button_1         = 'Sim'(100)
      icon_button_1         = 'ICON_OKAY '
      text_button_2         = 'Não'(101)
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = w_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF w_answer = '2'. "não
    EXIT.
  ENDIF.


  .
  LOOP AT tg_selectedrow INTO wg_selectedrow.


    REFRESH: it_return_ped, it_poitem_ped, it_poitemx_ped,
          it_bapimepotextheader_ped, it_pocond,it_pocondx.
    CLEAR: wa_return_ped, wa_poitem_ped, wa_poitemx_ped, wa_pocond,wa_pocondx,
           wa_poheader_ped, wa_poheaderx_ped, wa_bapimepotextheader_ped.
    "
    READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.
    SELECT SINGLE *
     FROM kna1
     INTO @DATA(_w_kna1)
     WHERE kunnr = @wa_saida-kunnr.
    IF _w_kna1-ktokd NE 'ZCIC'.
      CONTINUE.
    ENDIF.
    DATA(tabix) = sy-tabix.
    CLEAR wa_saida-message.
    "
    IF wa_saida-seq_lcto IS NOT INITIAL
      AND wa_saida-docnum IS NOT INITIAL
      AND wa_saida-doc_contab IS NOT INITIAL
      AND wa_saida-docs_estornados IS INITIAL
      AND wa_saida-nfenum IS NOT INITIAL
      AND wa_saida-ebeln IS INITIAL.

      SELECT SINGLE bukrs
      FROM j_1bbranch
      INTO v_bukrs
      WHERE branch = wa_saida-kunnr+6(4).

      "ITENS
      wa_poitem_ped-po_item       = 10.                                        "Item
      wa_poitem_ped-acctasscat    = 'K'.                                       "Categoria de classificação contábil
      CLEAR: VG_MATNR18.
      VG_MATNR18     = |{ wa_saida-matnr ALPHA = OUT }|.
      VG_MATNR18     = |{ VG_MATNR18     ALPHA = IN  }|.
      wa_saida-matnr = VG_MATNR18.

      wa_poitem_ped-material      = wa_saida-matnr.                            "Material
      wa_poitem_ped-quantity      = 1.                                         "Quantidade
      wa_poitem_ped-po_price      = 1.                                         "Transferência do preço: 1 = bruto, 2 = líquido
      wa_poitem_ped-price_unit    = 1.                                         "Unidade de preço
      wa_poitem_ped-orderpr_un    = 'MWH'.                                     "Unidade do preço do pedido
      IF wa_saida-bukrs NE '0028'.
        wa_poitem_ped-tax_code      = 'I0'.                                    "Código do Imposto
        wa_poitem_ped-net_price     = wa_saida-tarifa * wa_saida-montante.     "Preço
        wa_poitem_ped-net_price     = wa_poitem_ped-net_price - ( wa_poitem_ped-net_price * '0.0925' ).                         "Preço
      ELSE.
        wa_poitem_ped-tax_code      = 'I7'.                                    "Código do Imposto
        wa_poitem_ped-net_price     = wa_saida-tarifa * wa_saida-montante.                         "Preço
      ENDIF.
      wa_poitem_ped-plant         = wa_saida-kunnr+6(4).                       "Centro

*  wa_poitem_ped-stge_loc      = ''.                                        "Depósito
*  wa_poitem_ped-batch         = ''.                                        "lote
      APPEND wa_poitem_ped TO it_poitem_ped.
      wa_poitemx_ped-po_item      = 10.                                        "Item
      wa_poitemx_ped-po_itemx     = 'X'.                                       "Item
      wa_poitemx_ped-acctasscat   = 'X'.                                       "Categoria de classificação contábil
      wa_poitemx_ped-material     = 'X'.                                       "Material
      wa_poitemx_ped-quantity     = 'X'.                                       "Quantidade
      wa_poitemx_ped-po_price     = 'X'.                                       "Transferência do preço: 1 = bruto, 2 = líquido
      wa_poitemx_ped-price_unit   = 'X'.                                       "Unidade de preço
      wa_poitemx_ped-orderpr_un   = 'X'.                                       "Unidade do preço do pedido
      wa_poitemx_ped-net_price    = 'X'.                                       "Preço
      wa_poitemx_ped-tax_code     = 'X'.                                       "Código do Imposto
      wa_poitemx_ped-plant        = 'X'.                                       "Centro
*  wa_poitemx_ped-stge_loc     = 'X'.                                       "Depósito
*  wa_poitemx_ped-batch        = 'X'.                                       "lote
      APPEND wa_poitemx_ped TO it_poitemx_ped.
      "
      wa_poaccount-po_item      = 10.
      wa_poaccount-gl_account   = '0000414000'.
      "
      SELECT SINGLE csks~kostl
        FROM csks
        INNER JOIN cskt
        ON  cskt~spras = @sy-langu
        AND cskt~kostl = csks~kostl
        AND cskt~mctxt LIKE 'COMERCIAL%'
        INTO @DATA(_vkostl)
        WHERE csks~datbi = '99991231'
        AND   csks~kosar  = 'E'
        AND   csks~bukrs  = @v_bukrs
        AND   csks~gsber  = @wa_saida-kunnr+6(4).
      wa_poaccount-costcenter   = _vkostl.
      APPEND wa_poaccount TO it_poaccount.

      wa_poaccountx-po_item      = 10.
      wa_poaccountx-gl_account   = 'X'.
      wa_poaccountx-costcenter   = 'X'.
      APPEND wa_poaccountx TO it_poaccountx.

      "Cabeçalho
      wa_poheader_ped-comp_code = v_bukrs.                  "Empresa
      wa_poheader_ped-doc_type  = 'PCI'.                    "Tipo de Pedido

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida-branch
        IMPORTING
          output = wa_poheader_ped-vendor.

      SELECT SINGLE ekorg
         FROM t024w
         INTO v_ekorg
         WHERE werks EQ wa_saida-branch.
      wa_poheader_ped-purch_org = v_ekorg.                  "Organização de Compras
      wa_poheader_ped-doc_date  = sy-datum.                 "Data do Pedido
      wa_poheader_ped-langu     = sy-langu.                 "Idioma
      wa_poheader_ped-pur_group = 'D14'.                    "Grupo de Compradores
      wa_poheader_ped-currency  = 'BRL'.                    "Moeda pela ZMM0045
      wa_poheader_ped-exch_rate = 1.                        "Taxa de Câmbio pela ZMM0045
*  wa_poheader_ped-our_ref   = .          "Safra
*  wa_poheader_ped-quot_date = .                        "data cotação
      wa_poheader_ped-ref_1     = wa_saida-seq_lcto.        "Nº seq.

      wa_poheader_ped-pmnttrms    = 'Z001'.
      wa_poheader_ped-incoterms1  = 'CIF'.
      wa_poheader_ped-incoterms2  = 'CIF'.
      wa_poheader_ped-collect_no  = wa_saida-seq_lcto.   "Nº seq


      wa_poheaderx_ped-comp_code  = 'X'.                     "Empresa
      wa_poheaderx_ped-doc_type   = 'X'.                     "Tipo de Pedido
      wa_poheaderx_ped-vendor     = 'X'.                     "Fornecedor pela ZMM0045
      wa_poheaderx_ped-purch_org  = 'X'.                     "Organização de Compras
      wa_poheaderx_ped-doc_date   = 'X'.                     "Data do Pedido
      wa_poheaderx_ped-langu      = 'X'.                     "Idioma
      wa_poheaderx_ped-pur_group  = 'X'.                     "Grupo de Compradores
      wa_poheaderx_ped-currency   = 'X'.                     "Moeda pela ZMM0045
      wa_poheaderx_ped-exch_rate  = 'X'.                     "Taxa pela ZMM0045
*      wa_poheaderx_ped-our_ref    = 'X'.                     "Safra
*      wa_poheaderx_ped-quot_date  = 'X'.                     "Data Cotação
      wa_poheaderx_ped-ref_1      = 'X'.
      wa_poheaderx_ped-pmnttrms   = 'X'.
      wa_poheaderx_ped-incoterms1 = 'X'.
      wa_poheaderx_ped-incoterms2 = 'X'.
      wa_poheaderx_ped-collect_no = 'X'.                      "Nº seq
      "
      CALL FUNCTION 'BAPI_PO_CREATE1'
        EXPORTING
          poheader         = wa_poheader_ped
          poheaderx        = wa_poheaderx_ped
        IMPORTING
          exppurchaseorder = purchaseorder
        TABLES
          return           = it_return_ped
          poitem           = it_poitem_ped
          poitemx          = it_poitemx_ped
          poaccount        = it_poaccount
          poaccountx       = it_poaccountx
          pocond           = it_pocond
          pocondx          = it_pocondx
          popartner        = it_popartner_ped
          potextheader     = it_bapimepotextheader_ped.

      READ TABLE it_return_ped INTO wa_return_ped WITH KEY type = 'S' id = '06' number = '017'.
      IF sy-subrc EQ 0.
        "Commit
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        IF  purchaseorder IS NOT INITIAL.
          wa_saida-ebeln = purchaseorder.
          CONCATENATE  icon_checked wa_saida-ebeln INTO wa_saida-ebeln_flag SEPARATED BY ' - '.
          MODIFY it_saida FROM wa_saida INDEX tabix.
          UPDATE zfiwrt0022 SET ebeln_en = purchaseorder
               WHERE contrato EQ wa_saida-contrato
                AND  bukrs    EQ wa_saida-bukrs
                AND  ano      EQ p_ano
                AND  mes      EQ p_mes.
          "
          SELECT SINGLE * FROM zfiwrt0008 INTO @DATA(wa_t0008)
               WHERE seq_lcto EQ @wa_saida-seq_lcto.
          IF wa_t0008-docnum IS NOT INITIAL.
            SELECT SINGLE *
            FROM j_1bnfe_active
             INTO @DATA(wl_active)
              WHERE docnum EQ @wa_t0008-docnum.
            IF sy-subrc = 0.
              CONCATENATE wl_active-regio wl_active-nfyear wl_active-nfmonth wl_active-stcd1
                  wl_active-model wl_active-serie  wl_active-nfnum9  wl_active-docnum9 wl_active-cdv
                  INTO DATA(v_chave_doc).
              UPDATE zib_nfe_dist_ter SET ebeln  = purchaseorder
               WHERE chave_nfe EQ v_chave_doc.
            ENDIF.
          ENDIF.
          COMMIT WORK.

        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        "
        CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
          TABLES
            table    = it_return_ped
          EXCEPTIONS
            fb_error = 1
            OTHERS   = 2.
      ENDIF.
    ELSEIF  wa_saida-ebeln IS  NOT INITIAL.
      wa_saida-message = 'Pedido já gerado!'.
      MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING message.
    ELSEIF  wa_saida-docnum IS  INITIAL.
      wa_saida-message = 'Documento fiscal não gerado!'.
      MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING message.
    ELSEIF wa_saida-docs_estornados = 'X'.
      wa_saida-message = 'Documento fiscal estornado!'.
      MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING message.
    ELSEIF  wa_saida-doc_contab IS  INITIAL.
      wa_saida-message = 'Documento contabil não gerado!'.
      MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING message.
    ELSEIF vebeln_flag =  wa_saida-ebeln_flag.
      wa_saida-message = 'Aguarde retorno SEFAZ'.
      MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING message.
    ENDIF.

    CLEAR wa_saida.

  ENDLOOP.
ENDFORM.
"CS2021000785 Automatizar Pedidos de Compra/Lançtos NFs

**************************************************************************
* prepara contabil
**************************************************************************
FORM f_prepara_contabil.

  REFRESH tg_impo.

  LOOP AT it_zfiwrt0002 INTO wa_zfiwrt0002 WHERE operacao = wa_zfiwrt0008-operacao.
    READ TABLE it_j_1baj INTO wa_j_1baj WITH KEY taxtyp = wa_zfiwrt0002-taxtyp.

    READ TABLE it_j_1bajt INTO wa_j_1bajt  WITH KEY taxtyp = wa_zfiwrt0002-taxtyp.

    MOVE: wa_zfiwrt0002-taxtyp     TO tg_impo-taxtyp,
          wa_j_1bajt-ttypetxt      TO tg_impo-ttypetxt,
          wa_j_1baj-taxgrp         TO tg_impo-taxgrp.

    APPEND tg_impo.
    CLEAR: tg_impo.
  ENDLOOP.
  "
  REFRESH: tg_contab, tl_impo_aux.

  LOOP AT it_zfiwrt0003 INTO DATA(tl_0003) WHERE operacao = wa_zfiwrt0008-operacao.
    READ TABLE it_skat INTO DATA(tl_skat)
      WITH KEY saknr = tl_0003-hkont.

    READ TABLE it_tbsl INTO DATA(tl_tbsl)
      WITH KEY bschl = tl_0003-bschl.

    IF tl_tbsl-koart EQ 'K'
    OR tl_tbsl-koart EQ 'D'.

      MOVE wa_saida-kunnr TO tg_contab-hkont.
      MOVE: v_zfbdt TO tg_contab-zfbdt,
            'U'     TO tg_contab-zlsch.
    ELSE.
      MOVE: tl_0003-hkont   TO tg_contab-hkont.
    ENDIF.
    MOVE: tl_0003-bschl   TO tg_contab-bschl,
          tl_skat-txt50   TO tg_contab-txt50,
          tl_0003-taxtyp  TO tg_contab-taxtyp,
          tl_0003-estorno TO tg_contab-estorno,
          tl_0003-newbw   TO tg_contab-newbw,
          tl_0003-umskz   TO tg_contab-umskz.

    APPEND tg_contab.
    CLEAR: tg_contab.
  ENDLOOP.

  PERFORM monta_contabil.

  LOOP AT tg_contab.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = tg_contab-hkont
      IMPORTING
        output = tg_contab-hkont.
    "
    MOVE: sy-mandt                TO wa_zfiwrt0011-mandt,
          wa_zfiwrt0008-seq_lcto  TO wa_zfiwrt0011-seq_lcto,
          tg_contab-bschl         TO wa_zfiwrt0011-bschl,
          tg_contab-hkont         TO wa_zfiwrt0011-hkont,
          tg_contab-taxtyp        TO wa_zfiwrt0011-taxtyp,
          tg_contab-dmbtr         TO wa_zfiwrt0011-dmbtr,
          tg_contab-estorno       TO wa_zfiwrt0011-estorno,
          tg_contab-zlsch         TO wa_zfiwrt0011-zlsch,
          tg_contab-zfbdt         TO wa_zfiwrt0011-zfbdt,
          tg_contab-kostl         TO wa_zfiwrt0011-kostl,
          tg_contab-umskz         TO wa_zfiwrt0011-umskz,
          tg_contab-vbund         TO wa_zfiwrt0011-vbund.

    wa_zfiwrt0011-buzei  = sy-tabix.

    APPEND wa_zfiwrt0011 TO it_zfiwrt0011.
    CLEAR: wa_zfiwrt0011.
  ENDLOOP.

  LOOP AT tl_impo_aux.
    MOVE: sy-mandt               TO wa_zfiwrt0010-mandt,
          wa_zfiwrt0008-seq_lcto TO wa_zfiwrt0010-seq_lcto,
          wa_zfiwrt0009-itmnum   TO wa_zfiwrt0010-itmnum,
          tl_impo_aux-taxtyp     TO wa_zfiwrt0010-taxtyp,
          tl_impo_aux-base       TO wa_zfiwrt0010-base,
          tl_impo_aux-rate       TO wa_zfiwrt0010-rate,
          tl_impo_aux-taxval     TO wa_zfiwrt0010-taxval,
          tl_impo_aux-excbas     TO wa_zfiwrt0010-excbas,
          tl_impo_aux-othbas     TO wa_zfiwrt0010-othbas.
    APPEND wa_zfiwrt0010 TO it_zfiwrt0010.
  ENDLOOP.

ENDFORM.

**************************************************************************
* monta mensagem
**************************************************************************
FORM f_monta_mensagem CHANGING v_zfbdt.

  DATA: v_data_aux   TYPE sy-datum,
        wl_cont      TYPE sy-tabix,
        wl_linha     TYPE sy-tabix,
        wl_cont_aux  TYPE sy-tabix,
        wl_cont_aux2 TYPE sy-tabix.

  DATA: l_data   TYPE  p0001-begda,
        l_days   TYPE  t5a4a-dlydy,
        l_months TYPE  t5a4a-dlymo,
        l_signum TYPE  t5a4a-split,
        l_years  TYPE  t5a4a-dlyyr.

  FREE: tg_mensagens,
        it_zfiwrt0005.

  "texto
  wl_linha = 0.
  wl_cont = strlen( wa_saida-texto_nota ).
  wl_cont_aux = wl_cont / 72.
  DO.
    wl_linha  = sy-index.
    tg_mensagens-linnum    =  '01'.
    tg_mensagens-seqnum    =  wl_linha.
    MOVE: wa_saida-texto_nota+wl_cont_aux2 TO tg_mensagens-message.
    "
    APPEND tg_mensagens.
    "
    ADD 72 TO wl_cont_aux2.
    IF wl_cont_aux2 GT wl_cont.
      EXIT.
    ENDIF.
  ENDDO.

  l_data = p_data-low.

  IF wa_saida-pgto_mes_seguinte IS NOT INITIAL.

    l_months = 1.
    l_signum = '+'.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = l_data
        days      = l_days
        months    = l_months
        signum    = l_signum
        years     = l_years
      IMPORTING
        calc_date = l_data.
  ENDIF.

  IF wa_saida-vencimento GT 0. "X dia útil
*            CONCATENATE p_data-low+0(06) '01' INTO v_zfbdt.
    CONCATENATE l_data+0(06) '01' INTO v_zfbdt.
    v_data_aux = v_zfbdt.
    DO wa_saida-vencimento TIMES.
      "Jogar para o dia útil anterior
      zcl_miro=>get_proximo_dia_util(
        EXPORTING
          i_data_base = v_data_aux
          i_signum    = '+'
        RECEIVING
          r_data      = v_data_aux
        EXCEPTIONS
          erro        = 1
          OTHERS      = 2 ).

      IF v_data_aux IS NOT INITIAL.
        v_zfbdt = v_data_aux.
      ENDIF.
      ADD 1 TO v_data_aux.
    ENDDO.
  ELSE.
    CONCATENATE l_data+0(06) '01' INTO v_zfbdt.
*            CONCATENATE p_data-low+0(06) '01' INTO v_zfbdt.
    v_zfbdt = v_zfbdt + wa_saida-vencimento_f.
    v_data_aux = v_zfbdt - 1.

    "Jogar para o dia útil anterior
    zcl_miro=>get_proximo_dia_util(
      EXPORTING
        i_data_base = v_data_aux
        i_signum    = '+'
      RECEIVING
        r_data      = v_data_aux
      EXCEPTIONS
        erro        = 1
        OTHERS      = 2 ).

    IF v_data_aux IS NOT INITIAL.
      v_zfbdt = v_data_aux.
    ENDIF.
  ENDIF.

  ADD 1 TO wl_linha.
  tg_mensagens-linnum    =  '01'.
  tg_mensagens-seqnum    =  wl_linha.
*          CONCATENATE 'COMP.' ''  INTO TG_MENSAGENS-MESSAGE .
*          CONCATENATE TG_MENSAGENS-MESSAGE P_DATA-LOW+4(2) '/' P_DATA-LOW+0(4)    INTO TG_MENSAGENS-MESSAGE.
  CONCATENATE 'VENCIMENTO:' ''                     INTO tg_mensagens-message SEPARATED BY space.
  APPEND tg_mensagens.

  ADD 1 TO wl_linha.
  tg_mensagens-linnum    =  '01'.
  tg_mensagens-seqnum    =  wl_linha.
  CONCATENATE v_zfbdt+6(2) '/' v_zfbdt+4(2) '/' v_zfbdt+0(4)              INTO tg_mensagens-message.
  CONCATENATE  tg_mensagens-message wa_saida-banco '- AG:' wa_saida-agencia_banc 'C/C:' wa_saida-conta_banc INTO tg_mensagens-message SEPARATED BY space.
  APPEND tg_mensagens.

  LOOP AT tg_mensagens.
    MOVE-CORRESPONDING tg_mensagens TO wa_zfiwrt0005.
    MOVE wa_zfiwrt0008-operacao     TO wa_zfiwrt0005-operacao.
    APPEND wa_zfiwrt0005            TO it_zfiwrt0005.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_ATUALIZAR_NF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_envia_sefaz .
  "
  DATA vnfnum_flag(20).

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  IF tg_selectedrow[] IS INITIAL.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'E'.
    EXIT.
  ENDIF.
  LOOP AT tg_selectedrow INTO wg_selectedrow.
    READ TABLE it_saida INTO DATA(wsaida) INDEX wg_selectedrow-index.
    DATA(tabix) = sy-tabix.
    CLEAR wsaida-message.
    "
    vnfnum_flag = wsaida-nfenum.
    CONDENSE vnfnum_flag NO-GAPS.
    "
    SHIFT vnfnum_flag LEFT DELETING LEADING '0'.
    CONCATENATE  icon_activity vnfnum_flag INTO vnfnum_flag SEPARATED BY ' - '.

    MODIFY it_saida FROM wsaida INDEX tabix TRANSPORTING message.
    IF wsaida-seq_lcto IS NOT INITIAL
      AND wsaida-docnum IS NOT INITIAL
      AND wsaida-doc_contab IS NOT INITIAL
      AND wsaida-docs_estornados IS INITIAL
      AND wsaida-nfenum IS INITIAL
      AND vnfnum_flag NE  wa_saida-nfnum_flag.
      TRY.
          zcl_nfe=>zif_doc_eletronico~get_instance(
          EXPORTING
            i_docnum = wsaida-docnum
          )->set_registro(
            EXPORTING
              i_docnum       =  wsaida-docnum
              i_sem_bloqueio = abap_true
          )->set_autorizar(
          EXPORTING
            i_aguardar = abap_false

          )->get_ck_autorizado_uso(

          )->get_registro(
          IMPORTING
            e_documento = DATA(e_documento)
          )->set_clear( ).

        CATCH zcx_doc_eletronico INTO DATA(ex_doc_eletronico).
*          EX_DOC_ELETRONICO->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
          msg_id   = ex_doc_eletronico->msgid.
          msg_no   = ex_doc_eletronico->msgno.
          msg_var1 = ex_doc_eletronico->msgv1.
          msg_var2 = ex_doc_eletronico->msgv2.
          msg_var3 = ex_doc_eletronico->msgv3.
          msg_var4 = ex_doc_eletronico->msgv4.
          CALL FUNCTION 'MESSAGE_PREPARE'
            EXPORTING
              language = 'P'
              msg_id   = msg_id
              msg_no   = msg_no
              msg_var1 = msg_var1
              msg_var2 = msg_var2
              msg_var3 = msg_var3
              msg_var4 = msg_var4
            IMPORTING
              msg_text = wmessage.

          wsaida-message = wmessage.
          MODIFY it_saida FROM wsaida INDEX tabix TRANSPORTING message.
      ENDTRY.

      IF  e_documento-nfenum IS NOT INITIAL.
        wsaida-nfenum = e_documento-nfenum.
        MODIFY it_saida FROM wsaida INDEX tabix.
      ELSE.
*        MESSAGE 'Ocorreu um erro ao gerar a NFE!' TYPE 'I'.
*        EXIT.
      ENDIF.
    ELSEIF  wsaida-docnum IS  INITIAL.
      wsaida-message = 'Documento fiscal não gerado!'.
      MODIFY it_saida FROM wsaida INDEX tabix TRANSPORTING message.
    ELSEIF wsaida-docs_estornados = 'X'.
      wsaida-message = 'Documento fiscal estornado!'.
      MODIFY it_saida FROM wsaida INDEX tabix TRANSPORTING message.
    ELSEIF  wsaida-doc_contab IS  INITIAL.
      wsaida-message = 'Documento contabil não gerado!'.
      MODIFY it_saida FROM wsaida INDEX tabix TRANSPORTING message.
    ELSEIF vnfnum_flag =  wsaida-nfnum_flag.
      wsaida-message = 'Aguarde retorno SEFAZ'.
      MODIFY it_saida FROM wsaida INDEX tabix TRANSPORTING message.
    ELSEIF wsaida-nfenum IS NOT INITIAL.
      wsaida-message = 'Já enviado a SEFAZ'.
      MODIFY it_saida FROM wsaida INDEX tabix TRANSPORTING message.
    ENDIF.

    CLEAR wsaida.
  ENDLOOP.

  PERFORM z_busca_dados_atualizado.
ENDFORM.

FORM get_next_number USING p_object
                           p_nr_range
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
  ELSE.
    wg_flag = 'X'.
  ENDIF.
ENDFORM.

FORM monta_impostos TABLES tl_impo STRUCTURE tg_impo
                    USING e_row.

  DATA: BEGIN OF wl_1btxic,
          rate TYPE j_1btxic3-rate,
          base TYPE j_1btxic3-base,
        END OF wl_1btxic.

  DATA: wl_itens     LIKE LINE  OF tg_itens,
        wl_1baa      TYPE j_1baa,
        wl_base_aux  TYPE j_1btxic3-base,
        wl_a924      TYPE a924,
        wl_konp      TYPE konp,
        wl_t001w     TYPE t001w,
        wl_1btxsdc   TYPE j_1btxsdc,
        wl_1btxpis   TYPE j_1btxpis,
        wl_1btxcof   TYPE j_1btxcof,
        wl_impo_comp LIKE LINE OF tg_impo_comp.


**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003

*data:   tg_impo_aux2  like table of tg_impo2 with header line,
*        wa_impo_aux2  like tg_impo2,
*        wa_impo_aux3  like tg_impo,
  DATA: v_line TYPE i,
        v_ics1 TYPE zfiwrt0010-taxval.
*
  v_ics1 = 0.
  v_line = 1.

** Fim Alteração feita por Alexandre

**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003

  READ TABLE tg_impo WITH KEY taxtyp = 'ICS1'.
*
  IF sy-subrc = 0.
    MOVE 99 TO tg_impo-line.
    MODIFY tg_impo INDEX sy-tabix TRANSPORTING line.
    SORT tg_impo BY line DESCENDING.
  ENDIF.



*  loop at tg_impo.
*
*    move tg_impo-taxtyp    to wa_impo_aux2-taxtyp.
*    move tg_impo-ttypetxt to wa_impo_aux2-ttypetxt.
*    move tg_impo-taxgrp   to wa_impo_aux2-taxgrp.
*    move tg_impo-base     to wa_impo_aux2-base.
*    move tg_impo-rate     to wa_impo_aux2-rate.
*    move tg_impo-taxval   to wa_impo_aux2-taxval.
*    move tg_impo-excbas   to wa_impo_aux2-excbas.
*    move tg_impo-othbas   to wa_impo_aux2-othbas.
*
*    if wa_impo_aux2-taxtyp = 'ICS1'.
*      wa_impo_aux2-line = 1.
*    else.
*      wa_impo_aux2-line = v_line + 1.
*    endif.
*    v_line = v_line + 1.
*    append wa_impo_aux2 to tg_impo_aux2.
*  endloop.
*
*  sort tg_impo_aux2 by line.
*  refresh tg_impo.
*  clear wa_impo_aux2.
*  loop at tg_impo_aux2.
*
*    move tg_impo_aux2-taxtyp   to wa_impo_aux3-taxtyp.
*    move tg_impo_aux2-ttypetxt to wa_impo_aux3-ttypetxt.
*    move tg_impo_aux2-taxgrp   to wa_impo_aux3-taxgrp.
*    move tg_impo_aux2-base     to wa_impo_aux3-base.
*    move tg_impo_aux2-rate     to wa_impo_aux3-rate.
*    move tg_impo_aux2-taxval   to wa_impo_aux3-taxval.
*    move tg_impo_aux2-excbas   to wa_impo_aux3-excbas.
*    move tg_impo_aux2-othbas   to wa_impo_aux3-othbas.
*
*   append wa_impo_aux3 to tg_impo.
*  endloop.
*  refresh tg_impo_aux2.
*  clear wa_impo_aux3.
*endif.
** Fim Alteração feita por Alexandre


  READ TABLE tg_itens INTO wl_itens INDEX 1.

  SELECT SINGLE * FROM j_1baa INTO wl_1baa
    WHERE nftype EQ wa_zfiwrt0001-nftype.


  IF ( wl_1baa-direct EQ '1'  ).
    CLEAR: wl_a924, wl_konp, wl_t001w, wl_1btxsdc.

    SELECT SINGLE * FROM j_1btxsdc INTO wl_1btxsdc
      WHERE taxcode EQ wa_zfiwrt0006-taxcode.

    LOOP AT tg_impo.
      READ TABLE tg_impo_comp INTO wl_impo_comp WITH KEY itmnum = wl_itens-itmnum
                                                         taxtyp = tg_impo-taxtyp BINARY SEARCH.
      IF sy-subrc EQ 0 AND wa_zfiwrt0001-complemento EQ 'S'.
        MOVE-CORRESPONDING: wl_impo_comp TO tl_impo.
        MOVE :  tg_impo-ttypetxt  TO tl_impo-ttypetxt,
                tg_impo-taxgrp    TO tl_impo-taxgrp.
        APPEND tl_impo.
      ELSEIF tg_impo[] IS NOT INITIAL.
        IF tg_impo-taxtyp EQ 'ICM3'.
          IF wa_zfiwrt0006-opertyp EQ 'I'.
            IF wl_1baa-entrad EQ 'X'.
              SELECT SINGLE rate base FROM j_1btxic3 INTO wl_1btxic
                WHERE land1    EQ 'BR'
                AND   shipfrom EQ wg_shipfrom
                AND   shipto   EQ wg_shipto
                AND   gruop    EQ '30'
                AND   value    EQ wa_zfiwrt0008-parid
                AND   value2   EQ wl_itens-matnr.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE rate base FROM j_1btxic3 INTO wl_1btxic
                  WHERE land1     EQ 'BR'
                  AND   shipfrom  EQ wg_shipfrom
                  AND   shipto    EQ wg_shipto
                  AND   gruop     EQ '40'
                  AND   value     EQ wa_zfiwrt0008-parid.

                IF sy-subrc IS NOT INITIAL.
                  IF wa_zfiwrt0008-parvw NE 'BR' AND
                     wa_zfiwrt0008-parvw NE 'AG'.
                    SELECT SINGLE rate base FROM j_1btxic2 INTO wl_1btxic
                     WHERE land1     EQ 'BR'
                       AND shipfrom  EQ wg_shipfrom
                       AND shipto    EQ wg_shipto
                       AND matnr     EQ wl_itens-matnr.
                  ENDIF.
                  IF sy-subrc IS NOT INITIAL.
                    SELECT SINGLE rate FROM j_1btxic1 INTO wl_1btxic
                      WHERE  land1     EQ 'BR'
                         AND shipfrom  EQ wg_shipfrom
                         AND shipto    EQ wg_shipto.
                  ENDIF.
                ENDIF.
              ENDIF.
            ELSE.
              SELECT SINGLE  rate base FROM j_1btxic3 INTO wl_1btxic
                WHERE land1    EQ 'BR'
                AND   shipfrom EQ wg_shipfrom
                AND   shipto   EQ wg_shipto
                AND   gruop    EQ '76'
                AND   value    EQ wa_zfiwrt0008-parid
                AND   value2   EQ wl_itens-matnr.

              IF sy-subrc IS NOT INITIAL.
                IF wa_zfiwrt0008-parid NE 'BR' AND wa_zfiwrt0008-parid NE 'AG'.
                  SELECT SINGLE rate base FROM j_1btxic2 INTO wl_1btxic
                    WHERE land1     EQ 'BR'
                    AND   shipfrom  EQ wg_shipfrom
                    AND   shipto    EQ wg_shipto
                    AND   matnr     EQ tg_itens-matnr.
                ENDIF.
                IF sy-subrc IS NOT INITIAL.
                  SELECT SINGLE rate FROM j_1btxic1  INTO wl_1btxic
                    WHERE land1 EQ 'BR'
                    AND   shipfrom  EQ wg_shipfrom
                    AND   shipto    EQ wg_shipto.
                ENDIF.
              ENDIF.
            ENDIF.
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            SELECT SINGLE * FROM t001w INTO wa_t001w
              WHERE werks EQ wl_itens-werks.
            IF sy-subrc IS INITIAL.
              IF ( wl_1baa-direct NE '1'  ).
                SELECT SINGLE * FROM a924 INTO wl_a924
                  WHERE kschl    EQ 'ZIVP'
                  AND  aland     EQ 'BR'
                  AND  txreg_sf  EQ wa_t001w-regio
                  AND  matnr     EQ wl_itens-matnr
                  AND  datab     LE sy-datum
                  AND  datbi     GE sy-datum.
                IF sy-subrc IS INITIAL.
                  SELECT SINGLE * FROM konp INTO wl_konp
                    WHERE knumh EQ wl_a924-knumh.
                ENDIF.
              ENDIF.
            ENDIF.
            IF wl_1btxic-base IS INITIAL.
              IF wl_konp-kbetr GT wl_itens-netpr.
                wl_itens-netwr = wl_itens-menge *   wl_konp-kbetr.
              ENDIF.
              tl_impo-base = wl_itens-netwr.
              tl_impo-taxval = ( tl_impo-base * ( wl_1btxic-rate / 100 ) ).
              tl_impo-othbas = 0.
            ELSE.
              IF wl_konp-kbetr GT wl_itens-netpr.
                wl_itens-netwr = wl_itens-menge *  wl_konp-kbetr.
              ENDIF.
              tl_impo-base = wl_itens-netwr * ( wl_1btxic-base / 100 ).
              tl_impo-taxval = tl_impo-base * ( wl_1btxic-rate / 100 ).
              tl_impo-othbas = wl_itens-netwr - tl_impo-base.
            ENDIF.
            tl_impo-rate = wl_1btxic-rate.
            IF wa_zfiwrt0001-complemento EQ 'S'.
              CLEAR: tl_impo-rate ,tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ELSEIF wa_zfiwrt0006-opertyp EQ 'I'.
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            MOVE: wl_itens-netwr TO tl_impo-excbas.
            IF wa_zfiwrt0001-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ELSEIF wa_zfiwrt0006-opertyp EQ 'N'.
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
            IF wa_zfiwrt0001-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ENDIF.
        ELSEIF   wl_1btxsdc-pis EQ 'X'
           AND tg_impo-taxtyp EQ 'IPIS'.


          SELECT SINGLE *  FROM j_1btxpis  INTO wl_1btxpis
          WHERE country EQ 'BR'
           AND  gruop   EQ '72'
           AND  value   EQ wl_itens-werks.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          IF sy-subrc IS INITIAL.
            tl_impo-base   = wl_itens-netwr + v_ics1. " antes -  wl-_itens-netwr - Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003 wl_itens-netwr.
            tl_impo-rate   = wl_1btxpis-rate.
            tl_impo-taxval = tl_impo-base * ( wl_1btxpis-rate / 100 ).
            tl_impo-othbas = 0.
          ELSE.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
          ENDIF.
          IF wa_zfiwrt0001-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxpis.

        ELSEIF wl_1btxsdc-cofins EQ 'X' AND tg_impo-taxtyp EQ 'ICOF'.
          SELECT SINGLE * FROM j_1btxcof INTO wl_1btxcof
            WHERE country EQ 'BR'
            AND   gruop   EQ '71'
            AND   value   EQ wl_itens-werks.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          IF sy-subrc IS INITIAL.
            tl_impo-base   = wl_itens-netwr + v_ics1. " antes -  wl-_itens-netwr - Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003 wl_itens-netwr.
            tl_impo-rate = wl_1btxcof-rate.
            IF tl_impo-base > 0 AND wl_1btxcof-rate > 0.
              tl_impo-taxval = tl_impo-base * ( wl_1btxcof-rate / 100 ).
            ENDIF.
            tl_impo-othbas = 0.
          ELSE.
            MOVE wl_itens-netwr TO tl_impo-othbas.
          ENDIF.

          IF wa_zfiwrt0001-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxcof.

        ELSEIF tg_impo-taxtyp EQ 'ICS1'.
          SELECT SINGLE * FROM j_1baa INTO wl_1baa
            WHERE itmtyp EQ wa_zfiwrt0001-itmtyp.

          IF wl_1baa-entrad EQ 'X'.
            SELECT SINGLE rate base FROM  j_1btxic3 INTO wl_1btxic
              WHERE land1    EQ 'BR'
              AND   shipfrom EQ  wg_shipfrom
              AND   shipto   EQ  wg_shipto
              AND   gruop    EQ '30'
              AND   value    EQ wa_zfiwrt0008-parid
              AND   value2   EQ wl_itens-matnr.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = 'BR'
                   AND shipfrom	=	wg_shipfrom
                   AND shipto	  =	wg_shipto
                   AND gruop    = '40'
                   AND value    = wa_zfiwrt0008-parid.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE rate FROM j_1btxic1 INTO wl_1btxic
                  WHERE land1    EQ  'BR'
                  AND   shipfrom EQ wg_shipfrom
                  AND   shipto   EQ wg_shipto.
              ENDIF.
            ENDIF.
          ELSE.
            SELECT SINGLE rate base FROM j_1btxic3 INTO wl_1btxic
              WHERE land1     EQ 'BR'
              AND   shipfrom  EQ wg_shipfrom
              AND   shipto    EQ wg_shipto
              AND   gruop     EQ '76'
              AND   value     EQ wa_zfiwrt0008-parid
              AND   value2    EQ wl_itens-matnr.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE rate FROM j_1btxic1  INTO wl_1btxic
               WHERE land1    EQ 'BR'
                AND shipfrom EQ wg_shipfrom
                AND shipto   EQ wg_shipto.
            ENDIF.
          ENDIF.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          tl_impo-rate = wl_1btxic-rate.
          IF wl_1btxic-base > 0 AND wl_1btxic-rate > 0.
            tl_impo-base = wl_itens-netwr / ( 1 - ( ( wl_1btxic-rate * ( wl_1btxic-base / 100 ) ) / 100 ) ). " DEVK9A0ZAE - Ajuste 06.01.2021 - Anderosn Oenning
          ENDIF.


          IF tl_impo-base > 0 AND  tl_impo-rate > 0.
            IF  wl_1btxic-base > 0.

**Inicio USER STORY #81382 - Anderson Oenning
              tl_impo-base   = tl_impo-base * ( wl_1btxic-base / 100 ).
              tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
            ELSE.
              tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
            ENDIF.
**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003
            v_ics1 = tl_impo-taxval.
**Fim USER STORY #81382 - Anderson Oenning
          ENDIF.

          IF wa_zfiwrt0001-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.

          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxic.
        ELSE.
          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          MOVE: wl_itens-netwr TO tl_impo-othbas.

          IF wa_zfiwrt0001-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                    tl_impo-excbas.
          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSEIF ( wl_1baa-direct EQ '2' ).

    CLEAR: wl_a924, wl_konp, wl_t001w, wl_1btxsdc.
*  WL_1BTXIC3 TYPE J_1BTXIC3.
    SELECT SINGLE *
      FROM j_1btxsdc
      INTO wl_1btxsdc
       WHERE taxcode EQ wa_zfiwrt0006-taxcode.

    LOOP AT tg_impo.
      READ TABLE tg_impo_comp INTO wl_impo_comp WITH KEY itmnum = wl_itens-itmnum
                                                         taxtyp = tg_impo-taxtyp BINARY SEARCH.
      IF ( sy-subrc EQ 0 AND  wa_zfiwrt0008-complemento EQ 'S' ).
        MOVE-CORRESPONDING: wl_impo_comp TO tl_impo.
        MOVE :  tg_impo-ttypetxt  TO tl_impo-ttypetxt,
                tg_impo-taxgrp    TO tl_impo-taxgrp.
        APPEND tl_impo.
      ELSEIF tg_impo[] IS NOT INITIAL.
        IF tg_impo-taxtyp EQ 'ICM3'.
          IF wa_zfiwrt0006-opertyp EQ 'T'.
            SELECT SINGLE *
              FROM j_1baa
              INTO wl_1baa
               WHERE itmtyp EQ wa_zfiwrt0008-itmtyp.

            IF wl_1baa-entrad EQ 'X'.
              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = 'BR'
                   AND shipfrom	=	wg_shipfrom
                   AND shipto	  =	wg_shipto
                   AND gruop    = '30'
                   AND value    = wa_zfiwrt0008-parid
                   AND value2	  =	wl_itens-matnr.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE rate base
                  FROM j_1btxic3
                  INTO wl_1btxic
                   WHERE land1    = 'BR'
                     AND shipfrom	=	wg_shipfrom
                     AND shipto	  =	wg_shipto
                     AND gruop    = '40'
                     AND value    = wa_zfiwrt0008-parid.

                IF sy-subrc IS NOT INITIAL.
                  IF wa_zfiwrt0008-parvw NE 'BR'
                  AND wa_zfiwrt0008-parvw NE 'AG'.
                    SELECT SINGLE rate base
                      FROM j_1btxic2
                      INTO wl_1btxic
                       WHERE land1    = 'BR'
                         AND shipfrom	=	wg_shipfrom
                         AND shipto	  =	wg_shipto
                         AND matnr    = wl_itens-matnr.
                  ENDIF.
                  IF sy-subrc IS NOT INITIAL.
                    SELECT SINGLE rate
                      FROM j_1btxic1
                      INTO wl_1btxic
                       WHERE land1    = 'BR'
                         AND shipfrom	=	wg_shipfrom
                         AND shipto	  =	wg_shipto.

                  ENDIF.
                ENDIF.
              ENDIF.

            ELSE.
              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = 'BR'
                   AND shipfrom	=	wg_shipfrom
                   AND shipto	  =	wg_shipto
                   AND gruop    = '76'
                   AND value    = wa_zfiwrt0008-parid
                   AND value2	  =	wl_itens-matnr.

              IF sy-subrc IS NOT INITIAL.
                IF wa_zfiwrt0008-parvw NE 'BR'
                AND wa_zfiwrt0008-parvw NE 'AG'.
                  SELECT SINGLE rate base
                    FROM j_1btxic2
                    INTO wl_1btxic
                     WHERE land1    = 'BR'
                       AND shipfrom	=	wg_shipfrom
                       AND shipto	  =	wg_shipto
                       AND matnr    = wl_itens-matnr.
                ENDIF.
                IF sy-subrc IS NOT INITIAL.
                  SELECT SINGLE rate
                    FROM j_1btxic1
                    INTO wl_1btxic
                     WHERE land1    = 'BR'
                       AND shipfrom = wg_shipfrom
                       AND shipto   = wg_shipto.
                ENDIF.
              ENDIF.
            ENDIF.
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            SELECT SINGLE *
              FROM t001w
              INTO wl_t001w
               WHERE werks EQ wl_itens-werks.
            IF sy-subrc IS INITIAL.


              SELECT SINGLE *
                FROM a924
                INTO wl_a924
                 WHERE kschl    EQ 'ZIVP'
                   AND aland    EQ 'BR'
                   AND txreg_sf EQ wl_t001w-regio
                   AND matnr    EQ wl_itens-matnr
                   AND datab    LE sy-datum
                   AND datbi    GE sy-datum.

              IF sy-subrc IS INITIAL.


                SELECT SINGLE *
                  FROM konp
                  INTO wl_konp
                   WHERE knumh EQ wl_a924-knumh.

              ENDIF.

            ENDIF.
            IF wl_1btxic-base IS INITIAL.
              IF wl_konp-kbetr GT wl_itens-netpr.
                wl_itens-netwr = wl_itens-menge * wl_konp-kbetr.
              ENDIF.
              tl_impo-base   = wl_itens-netwr.
              tl_impo-taxval = ( tl_impo-base * ( wl_1btxic-rate / 100 ) ).
              tl_impo-othbas = 0.

            ELSE.
              IF wl_konp-kbetr GT wl_itens-netpr.
                wl_itens-netwr = wl_itens-menge * wl_konp-kbetr.
              ENDIF.
              tl_impo-base   = wl_itens-netwr * ( wl_1btxic-base / 100 ).
              tl_impo-taxval = tl_impo-base * ( wl_1btxic-rate / 100 ).
              tl_impo-othbas = wl_itens-netwr - tl_impo-base.

            ENDIF.
            tl_impo-rate = wl_1btxic-rate.
            IF wa_zfiwrt0008-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ELSEIF wa_zfiwrt0006-opertyp EQ 'I'.
**  aqui outros tipos de operacoes
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            MOVE: wl_itens-netwr TO tl_impo-excbas.
            IF wa_zfiwrt0008-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ELSEIF wa_zfiwrt0006-opertyp EQ 'N'.
            MOVE-CORRESPONDING: tg_impo TO tl_impo.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
            IF wa_zfiwrt0008-complemento EQ 'S'.
              CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                     tl_impo-excbas.
            ENDIF.
            APPEND tl_impo.
            CLEAR: tl_impo.
          ENDIF.
        ELSEIF wl_1btxsdc-pis EQ 'X'
           AND tg_impo-taxtyp EQ 'IPIS'.

          SELECT SINGLE *
            FROM j_1btxpis
            INTO wl_1btxpis
             WHERE country EQ 'BR'
               AND gruop   EQ '72'
               AND value   EQ wl_itens-werks.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          IF sy-subrc IS INITIAL.
            tl_impo-base   = wl_itens-netwr + v_ics1. " antes -  wl-_itens-netwr - Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003 wl_itens-netwr.
            tl_impo-rate   = wl_1btxpis-rate.
            tl_impo-taxval = tl_impo-base * ( wl_1btxpis-rate / 100 ).
            tl_impo-othbas = 0.
          ELSE.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
          ENDIF.
          IF wa_zfiwrt0008-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxpis.

        ELSEIF wl_1btxsdc-cofins EQ 'X'
           AND tg_impo-taxtyp EQ 'ICOF'.
          SELECT SINGLE *
            FROM j_1btxcof
            INTO wl_1btxcof
             WHERE country EQ 'BR'
               AND gruop   EQ '71'
               AND value   EQ wl_itens-werks.

          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          IF sy-subrc IS INITIAL.
            tl_impo-base   = wl_itens-netwr + v_ics1. " antes -  wl-_itens-netwr - Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003 wl_itens-netwr.
            tl_impo-rate   = wl_1btxcof-rate.
            IF  tl_impo-base > 0 AND wl_1btxcof-rate  > 0.
              tl_impo-taxval = tl_impo-base * ( wl_1btxcof-rate / 100 ).
            ENDIF.
            tl_impo-othbas = 0.
          ELSE.
            MOVE: wl_itens-netwr TO tl_impo-othbas.
          ENDIF.

          IF wa_zfiwrt0008-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.
          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxcof.

        ELSEIF  tg_impo-taxtyp EQ 'ICS1'.
          SELECT SINGLE *
           FROM j_1baa
           INTO wl_1baa
            WHERE itmtyp EQ wa_zfiwrt0008-itmtyp.

          IF wl_1baa-entrad EQ 'X'.
            SELECT SINGLE rate base
              FROM j_1btxic3
              INTO wl_1btxic
               WHERE land1    = 'BR'
                 AND shipfrom	=	wg_shipfrom
                 AND shipto	  =	wg_shipto
                 AND gruop    = '30'
                 AND value    = wa_zfiwrt0008-parid
                 AND value2	  =	wl_itens-matnr.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE rate base
                FROM j_1btxic3
                INTO wl_1btxic
                 WHERE land1    = 'BR'
                   AND shipfrom	=	wg_shipfrom
                   AND shipto	  =	wg_shipto
                   AND gruop    = '40'
                   AND value    = wa_zfiwrt0008-parid.

              IF sy-subrc IS NOT INITIAL.
                SELECT SINGLE rate
                  FROM j_1btxic1
                  INTO wl_1btxic
                   WHERE land1    = 'BR'
                     AND shipfrom	=	wg_shipfrom
                     AND shipto	  =	wg_shipto.

              ENDIF.

            ENDIF.

          ELSE.
            SELECT SINGLE rate base
              FROM j_1btxic3
              INTO wl_1btxic
               WHERE land1    = 'BR'
                 AND shipfrom	=	wg_shipfrom
                 AND shipto	  =	wg_shipto
                 AND gruop    = '76'
                 AND value    = wa_zfiwrt0008-parid
                 AND value2	  =	wl_itens-matnr.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE rate
                FROM j_1btxic1
                INTO wl_1btxic
                 WHERE land1    = 'BR'
                   AND shipfrom = wg_shipfrom
                   AND shipto   = wg_shipto.
            ENDIF.

          ENDIF.
          MOVE-CORRESPONDING: tg_impo TO tl_impo.

          tl_impo-rate =  wl_1btxic-rate .
          IF wl_1btxic-base > 0 AND  wl_1btxic-rate > 0.
            tl_impo-base = wl_itens-netwr / ( 1 - ( ( wl_1btxic-rate * ( wl_1btxic-base / 100 ) ) / 100 ) ). " DEVK9A0ZAE - Ajuste 06.01.2021 - Anderosn Oenning
          ENDIF.


          IF tl_impo-base > 0 AND  tl_impo-rate > 0.
            IF  wl_1btxic-base > 0.
**Inicio USER STORY #81382 - Anderson Oenning
              tl_impo-base = tl_impo-base * ( wl_1btxic-base / 100 ).
              tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
            ELSE.
              tl_impo-taxval = tl_impo-base * ( tl_impo-rate / 100 ).
            ENDIF.
**Fim USER STORY #81382 - Anderson Oenning
**  Alteracao feita por Alexandre ref; CS1016278 - IR107256 - 27.02.2003
            v_ics1 = tl_impo-taxval.
          ENDIF.


          IF wa_zfiwrt0008-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.

          APPEND tl_impo.
          CLEAR: tl_impo, wl_1btxic.
        ELSE.

**        Aqui outros impostos
          MOVE-CORRESPONDING: tg_impo TO tl_impo.
          MOVE: wl_itens-netwr TO tl_impo-othbas.

          IF wa_zfiwrt0008-complemento EQ 'S'.
            CLEAR: tl_impo-rate, tl_impo-base, tl_impo-taxval, tl_impo-othbas,
                   tl_impo-excbas.
          ENDIF.

          APPEND tl_impo.
          CLEAR: tl_impo.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM monta_contabil.
  DATA:
    tl_impo  LIKE TABLE OF tg_impo WITH HEADER LINE,
    tg_tbsl  TYPE TABLE OF tbsl WITH HEADER LINE,
    t_hkont  TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
    wa_tka02 TYPE tka02,
    v_koart  TYPE tbsl-koart,
    v_vbund  TYPE bseg-vbund,
    wl_tabix TYPE sy-tabix,
    v_dmbtr  TYPE zfiwrt0011-dmbtr.

  REFRESH: tl_impo, tl_impo_aux.
  CLEAR: tl_impo, tl_impo_aux.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'CONTAS_EC-CS'
    TABLES
      set_values    = t_hkont
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  SELECT SINGLE *
    FROM tka02
    INTO wa_tka02
    WHERE bukrs = wa_zfiwrt0008-bukrs.

  CONCATENATE 'CE4' wa_tka02-kokrs '_ACCT' INTO DATA(tabco1).
  CONCATENATE 'CE4' wa_tka02-kokrs         INTO DATA(tabco2).

  tg_tbsl[] = it_tbsl[].
  LOOP AT tg_contab.
    READ TABLE tg_tbsl
    WITH KEY bschl = tg_contab-bschl.
    IF tg_tbsl-koart = 'D' OR tg_tbsl-koart = 'K'.
      v_koart = tg_tbsl-koart.
    ENDIF.
    MOVE: 0 TO tg_contab-dmbtr.
    MODIFY tg_contab.
  ENDLOOP.

  LOOP AT tg_itens.
    PERFORM monta_impostos TABLES tl_impo_aux
                            USING sy-tabix.

    LOOP AT tl_impo_aux.
      MOVE-CORRESPONDING tl_impo_aux TO tl_impo.
      COLLECT tl_impo.
    ENDLOOP.
  ENDLOOP.
  tl_impo_aux[] = tl_impo[].

  LOOP AT tg_contab.
    wl_tabix = sy-tabix.
    READ TABLE tg_tbsl
    WITH KEY bschl = tg_contab-bschl.

    "Sociedade Parceira
    CLEAR: tg_contab-vbund, v_vbund.
    READ TABLE t_hkont WITH KEY from = tg_contab-hkont.
    IF sy-subrc = 0.
      IF v_koart = 'D'.
        SELECT SINGLE vbund INTO v_vbund FROM kna1
          WHERE kunnr = wa_zfiwrt0008-parid "soc parceira do emissor
          AND   ktokd IN ('ZCIC','ZCEX','ZCPF', 'SCIC','ZCNJ').
      ELSEIF v_koart = 'K'.
        SELECT SINGLE vbund INTO v_vbund FROM lfa1
          WHERE lifnr = wa_zfiwrt0008-parid "soc parceira do emissor
          AND   ktokk IN ('ZFIC','ZFEX','ZPRF', 'SFIC', 'ZFNJ').
      ENDIF.
      tg_contab-vbund = v_vbund.
      MODIFY tg_contab INDEX wl_tabix TRANSPORTING vbund.
    ENDIF.


    CLEAR: tl_impo.
    IF tg_contab-taxtyp IS INITIAL.
      IF wa_zfiwrt0001-complemento = 'S'.
        LOOP AT tl_impo WHERE taxtyp EQ 'ICM3'.
          IF tg_tbsl-shkzg EQ 'H'.
            SUBTRACT tl_impo-taxval FROM tg_contab-dmbtr.
          ELSE.
            ADD tl_impo-taxval TO tg_contab-dmbtr.
          ENDIF.
        ENDLOOP.
      ENDIF.
      MODIFY tg_contab INDEX wl_tabix.
      IF wa_zfiwrt0001-energia EQ 'N'.
        LOOP AT tg_itens.
          IF tg_tbsl-shkzg EQ 'H'.
            SUBTRACT tg_itens-netwr FROM tg_contab-dmbtr.
          ELSE.
            ADD tg_itens-netwr TO tg_contab-dmbtr.
          ENDIF.
        ENDLOOP.

      ELSEIF wa_zfiwrt0001-energia EQ 'S'.
        LOOP AT tl_impo  WHERE taxtyp EQ 'ICS1'.
          IF tg_tbsl-shkzg EQ 'H'.
*====== "Inicio USER STORY 81382  "Anderson Oenning

*            SUBTRACT tl_impo-base FROM tg_contab-dmbtr.
            CLEAR: v_dmbtr.
            v_dmbtr = ( tg_itens-netwr + tl_impo-taxval ). "Valor total do item + imposto
            SUBTRACT v_dmbtr FROM tg_contab-dmbtr.
          ELSE.
*            ADD tl_impo-base TO tg_contab-dmbtr.
            CLEAR: v_dmbtr.
            v_dmbtr = ( tg_itens-netwr + tl_impo-taxval ). "Valor total do item + imposto
            ADD v_dmbtr TO tg_contab-dmbtr.
*====== "Inicio USER STORY 81382  "Anderson Oenning
          ENDIF.
        ENDLOOP.
      ENDIF.
      MODIFY tg_contab INDEX wl_tabix.
    ELSE.
      READ TABLE tl_impo WITH KEY taxtyp = tg_contab-taxtyp.
      IF sy-subrc IS INITIAL.
        IF tg_tbsl-shkzg EQ 'H'.
          MOVE: tl_impo-taxval TO tg_contab-dmbtr.
          MULTIPLY tg_contab-dmbtr BY -1.
        ELSE.
          MOVE tl_impo-taxval TO tg_contab-dmbtr.
        ENDIF.
        MODIFY  tg_contab INDEX wl_tabix.
      ENDIF.
    ENDIF.

    CLEAR: wl_tabix, tl_impo, tg_tbsl.
  ENDLOOP.
  SORT tg_contab BY taxtyp bschl.




ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_DADOS_ATUALIZADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_busca_dados_atualizado.
  SORT:
      tl_zib_chv BY obj_key,
      tl_zib_err BY obj_key,
      tl_zib_cont BY obj_key,
      tl_0011     BY seq_lcto.
  it_saida_aux[] = it_saida[].
  REFRESH tl_docest.
  LOOP AT it_saida_aux INTO wa_saida_aux.
    IF wa_saida_aux-seq_lcto IS NOT INITIAL.
      MOVE: wa_saida_aux-seq_lcto TO tl_docest-seq_lcto.

      APPEND tl_docest.
      CLEAR: tl_docest.
    ENDIF.
  ENDLOOP.

  SORT  tl_docest BY seq_lcto.

  CALL FUNCTION 'ZNFW_ESTORNA_SEQ_LCTO'
    TABLES
      t_docs = tl_docest.

  LOOP AT it_saida_aux INTO wa_saida_aux.
    REFRESH: wa_saida_aux-color.
    DATA(tabix) = sy-tabix.
    SELECT SINGLE *
      FROM zfiwrt0022 INTO @DATA(wa_t0022)
     WHERE contrato EQ @wa_saida_aux-contrato
      AND  bukrs    EQ @wa_saida_aux-bukrs
      AND  branch   EQ @wa_saida_aux-branch
      AND  kunnr    EQ @wa_saida_aux-kunnr
      AND  ano      EQ @wa_saida_aux-ano
      AND  mes      EQ @p_mes.


    IF wa_t0022-seq_lcto IS NOT INITIAL.

      SELECT SINGLE * FROM zfiwrt0008 INTO @DATA(wa_t0008)
        WHERE seq_lcto EQ @wa_t0022-seq_lcto.
      IF wa_t0008-docnum IS NOT INITIAL.
        SELECT SINGLE *
        FROM j_1bnfe_active
         INTO @DATA(wa_active)
          WHERE docnum EQ @wa_t0008-docnum.
        IF sy-subrc = 0.
          wa_saida_aux-docsta = wa_active-docsta.
          SELECT SINGLE *
            FROM j_1bnfdoc
            INTO @DATA(wa_nfdoc)
            WHERE docnum EQ @wa_t0008-docnum.
          "
          wa_saida_aux-nfenum = wa_nfdoc-nfenum.
          "
          IF wa_saida_aux-nfenum IS INITIAL.
            wa_saida_aux-nfnum_flag = icon_warning.
          ELSE.
            wa_saida_aux-nfnum_flag = wa_nfdoc-nfenum.
            CONDENSE wa_saida_aux-nfnum_flag NO-GAPS.
            "
            SHIFT wa_saida_aux-nfnum_flag LEFT DELETING LEADING '0'.
            IF wa_nfdoc-candat IS NOT INITIAL.
              CONCATENATE  icon_storno wa_saida_aux-nfnum_flag INTO wa_saida_aux-nfnum_flag SEPARATED BY ' - '.
            ELSEIF wa_active-cancel = 'X'. "Cancelado SEFAZ
              CONCATENATE  icon_cancel wa_saida_aux-nfnum_flag INTO wa_saida_aux-nfnum_flag SEPARATED BY ' - '.
              wa_saida_aux-cancel = 'X'.
            ELSEIF wa_active-docsta EQ space OR   wa_active-action_requ EQ space.
              CONCATENATE  icon_activity wa_saida_aux-nfnum_flag INTO wa_saida_aux-nfnum_flag SEPARATED BY ' - '.
            ELSEIF wa_active-docsta EQ '1'.
              CONCATENATE  icon_complete wa_saida_aux-nfnum_flag INTO wa_saida_aux-nfnum_flag SEPARATED BY ' - '.
            ELSE.
              CONCATENATE  icon_status_critical wa_saida_aux-nfnum_flag INTO wa_saida_aux-nfnum_flag SEPARATED BY ' - '.
            ENDIF.
          ENDIF.
        ENDIF.

        CLEAR: tl_docest,  wa_saida_aux-color.

        READ TABLE tl_docest
              WITH KEY seq_lcto = wa_t0008-seq_lcto
                  BINARY SEARCH.

        IF ( tl_docest-docnum_est IS NOT INITIAL
           AND tl_docest-docnum_est NE '0000000000' ) OR wa_t0008-loekz = 'X'.
          CLEAR: wl_color.
          wl_color-fieldname = 'DOCNUM_FLAG'.
          wl_color-color-col = 6.
          wl_color-color-inv = 6.
          APPEND wl_color TO wa_saida_aux-color.

          CLEAR: wl_color.
          wl_color-fieldname = 'SEQ_LCTO_FLAG'.
          wl_color-color-col = 6.
          wl_color-color-inv = 6.
          APPEND wl_color TO wa_saida_aux-color.
        ENDIF.

**    Doc.Contabíl
        IF tl_docest-belnr_est IS NOT INITIAL
        AND tl_docest-belnr_est NE '0000000000'.
          CLEAR: wl_color.
          wl_color-fieldname = 'DOC_CONTAB_FLAG'.
          wl_color-color-col = 6.
          wl_color-color-inv = 6.
          APPEND wl_color TO wa_saida_aux-color.
        ENDIF.
      ELSE.
        wa_saida_aux-docnum_flag = icon_warning.
        wa_saida_aux-doc_contab_flag = icon_warning.
        wa_saida_aux-nfnum_flag = icon_warning.
        CLEAR wa_saida_aux-message.

        IF  wa_t0008-loekz = 'X'.
*-CS2021000723 - #74615 - 19.04.2022 - JT - inicio
*         CLEAR: wl_color.
*         wl_color-fieldname = 'DOCNUM_FLAG'.
*         wl_color-color-col = 6.
*         wl_color-color-inv = 6.
*         APPEND wl_color TO wa_saida_aux-color.
*
*         CLEAR: wl_color.
*         wl_color-fieldname = 'SEQ_LCTO_FLAG'.
*         wl_color-color-col = 6.
*         wl_color-color-inv = 6.
*         APPEND wl_color TO wa_saida_aux-color.
*-CS2021000723 - #74615 - 19.04.2022 - JT - fim
        ENDIF.
      ENDIF.

      wa_saida_aux-docs_estornados = wa_t0008-docs_estornados.

      IF wa_t0008-docs_estornados = 'X'.
        UPDATE zfiwrt0022 SET data_fatura = ' '
             WHERE contrato EQ wa_saida_aux-contrato
              AND  bukrs    EQ wa_saida_aux-bukrs
              AND  ano      EQ p_ano
              AND  mes      EQ p_mes.

*        UPDATE ZFIWRT0008 SET LOEKZ = 'X'
*             WHERE SEQ_LCTO = WA_T0022-SEQ_LCTO.

        COMMIT WORK.
      ENDIF.

      SELECT SINGLE * FROM  zfiwrt1000 INTO @DATA(wa_t1000)
        WHERE seq_lcto EQ @wa_t0022-seq_lcto
          AND field    EQ 'SEQ_LCTO'.

      IF sy-subrc IS INITIAL.
        wa_saida_aux-seq_lcto_flag = icon_status_critical.
      ELSE.
        IF wa_t0008-seq_lcto IS INITIAL.
          wa_saida_aux-seq_lcto_flag = icon_warning.
        ELSE.
          SHIFT wa_t0008-seq_lcto LEFT DELETING LEADING '0'.
          CONCATENATE  icon_checked wa_t0008-seq_lcto INTO wa_saida_aux-seq_lcto_flag SEPARATED BY ' - '.
          wa_saida_aux-seq_lcto =  |{ wa_t0008-seq_lcto ALPHA = IN  }|.
        ENDIF.
      ENDIF.

      CLEAR wa_t1000.

      SELECT SINGLE * FROM  zfiwrt1000 INTO wa_t1000
             WHERE seq_lcto EQ wa_t0022-seq_lcto
               AND field    EQ 'DOCNUM'.

      IF sy-subrc IS INITIAL.
        wa_saida_aux-docnum_flag = icon_status_critical.
      ELSE.
        IF wa_t0008-docnum IS INITIAL.
          wa_saida_aux-docnum_flag = icon_warning.
        ELSE.
          SHIFT wa_t0008-docnum LEFT DELETING LEADING '0'.
          CONCATENATE  icon_checked wa_t0008-docnum INTO wa_saida_aux-docnum_flag SEPARATED BY ' - '.
          wa_saida_aux-docnum =  |{ wa_t0008-docnum ALPHA = IN  }|.
        ENDIF.
      ENDIF.

      READ TABLE tl_zib_err
           WITH KEY obj_key = wa_t0008-obj_key
             type    = 'E'.

      IF sy-subrc IS INITIAL.
        wa_saida_aux-doc_contab_flag = icon_status_critical.
      ELSE.
        CLEAR tl_zib_chv.
        READ TABLE tl_zib_chv
          WITH KEY obj_key = wa_t0008-obj_key BINARY SEARCH.
        IF tl_zib_chv-belnr IS INITIAL.
          READ TABLE tl_0011
          WITH KEY seq_lcto = wa_t0022-seq_lcto.
          IF sy-subrc IS INITIAL.
            wa_saida_aux-doc_contab_flag = icon_warning.
          ELSE.
            wa_saida_aux-doc_contab_flag =  icon_system_okay.
          ENDIF.
        ELSE.
          SHIFT tl_zib_chv-belnr LEFT DELETING LEADING '0'.
          CONCATENATE  icon_checked tl_zib_chv-belnr INTO wa_saida_aux-doc_contab_flag SEPARATED BY ' - '.
          wa_saida_aux-doc_contab = tl_zib_chv-belnr.
        ENDIF.
      ENDIF.

*-CS2021000723 - #74615 - 19.04.2022 - JT - inicio
      IF wa_t0008-docnum IS INITIAL AND wa_t0008-loekz = 'X'.
        wa_saida_aux-seq_lcto           = abap_off.
        wa_saida_aux-seq_lcto_flag      = abap_off.
        wa_saida_aux-docnum_flag        = abap_off.
        wa_saida_aux-doc_contab_flag    = abap_off.
        wa_saida_aux-nfnum_flag         = abap_off.

        DELETE FROM zfiwrt0022 WHERE contrato EQ wa_saida_aux-contrato
                                 AND bukrs    EQ wa_saida_aux-bukrs
                                 AND branch   EQ wa_saida_aux-branch
                                 AND ano      EQ p_ano
                                 AND mes      EQ p_mes.
        COMMIT WORK.
      ENDIF.
*-CS2021000723 - #74615 - 19.04.2022 - JT - fim

      MODIFY it_saida      FROM wa_saida_aux INDEX tabix.
      MODIFY it_saida_aux  FROM wa_saida_aux INDEX tabix.
    ENDIF.

    CLEAR: wa_saida_aux, wa_t0008, wa_t0022.
  ENDLOOP.
ENDFORM.

FORM chama_log_bapi TABLES tl_return STRUCTURE zfiwrt1000.
  DATA: BEGIN OF tl_log_bapi OCCURS 0,
          tipo      TYPE c,
          line(255) TYPE c,
        END OF tl_log_bapi.

  DATA: wl_layout TYPE slis_layout_alv.

  LOOP AT tl_return.
    MOVE: tl_return-type       TO tl_log_bapi-tipo,
           tl_return-mensagem  TO tl_log_bapi-line.
    APPEND tl_log_bapi.

  ENDLOOP.

  REFRESH estrutura.
  PERFORM monta_layout_log.

  wl_layout-zebra = 'X'.
  wl_layout-colwidth_optimize = 'X'.

  IF tl_log_bapi[] IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program    = sy-repid
        it_fieldcat           = estrutura[]
        is_layout             = wl_layout
        i_save                = 'A'
        i_screen_start_column = 10
        i_screen_start_line   = 3
        i_screen_end_column   = 80
        i_screen_end_line     = 20
      TABLES
        t_outtab              = tl_log_bapi.

  ENDIF.

ENDFORM.


FORM monta_layout_log.
  PERFORM monta_estrutura_log  USING :
        1 ' '  ' ' 'TL_LOG_BAPI' 'TIPO'  'Tipo de Msg.'  ' ',
        2 ' '  ' ' 'TL_LOG_BAPI' 'LINE'  'Log. de Exec.'  ' ' .

ENDFORM.

FORM monta_estrutura_log USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen).

  CLEAR wa_estrutura.
  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura


FORM f_preencher_dynpro USING l_start TYPE c
                              l_name  TYPE c l_value.

  MOVE l_start TO wl_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE: l_name TO wl_bdc-program,
          l_value TO wl_bdc-dynpro.
  ELSE.
    MOVE: l_name TO wl_bdc-fnam,
          l_value TO wl_bdc-fval.
  ENDIF.
  APPEND wl_bdc TO tl_bdc.
  CLEAR: wl_bdc.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_ESTORNAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_estornar .

  DATA: wl_answer,
        tabix                 TYPE sy-tabix,
        e_info_doc_eletronico TYPE j_1bnfe_active,
        tl_docs               TYPE TABLE OF zfiwrs0003,
        vnfnum_flag(20).

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question  = 'Tem certeza que deseja processar esse(s) documento(s)?'
    IMPORTING
      answer         = wl_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  CASE wl_answer.
    WHEN '2' OR 'A'.
      EXIT.
  ENDCASE.

  LOOP AT tg_selectedrow INTO wg_selectedrow.

    READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.
    tabix = sy-tabix.
    " Cancela NFE
    vnfnum_flag = wa_saida-nfenum.
    CONDENSE vnfnum_flag NO-GAPS.
    "
    SHIFT vnfnum_flag LEFT DELETING LEADING '0'.
    CONCATENATE  icon_activity vnfnum_flag INTO vnfnum_flag SEPARATED BY ' - '.
    "
    CLEAR e_info_doc_eletronico.
    IF wa_saida-seq_lcto IS NOT INITIAL
       AND wa_saida-docnum IS NOT INITIAL
       AND wa_saida-docs_estornados IS INITIAL
       AND wa_saida-ebeln IS INITIAL
       AND vnfnum_flag NE  wa_saida-nfnum_flag.
      IF wa_saida-cancel NE 'X' AND wa_saida-nfenum IS NOT INITIAL AND wa_saida-docsta = '1'.
        TRY.
            zcl_nfe=>zif_doc_eletronico~get_instance(
            EXPORTING
              i_docnum = wa_saida-docnum
            )->set_registro(
              EXPORTING
                i_docnum       = wa_saida-docnum
                i_sem_bloqueio = abap_true
            )->set_cancelar(
            EXPORTING
              i_aguardar = abap_false
              i_motivo   = '01' "VALOR INCORRETO

            )->get_ck_autorizado_uso(

            )->get_registro(
            IMPORTING
              e_documento = DATA(e_documento)
              e_info_doc_eletronico = e_info_doc_eletronico
            )->set_clear( ).

          CATCH zcx_doc_eletronico INTO DATA(ex_doc_eletronico).
            msg_id   = ex_doc_eletronico->msgid.
            msg_no   = ex_doc_eletronico->msgno.
            msg_var1 = ex_doc_eletronico->msgv1.
            msg_var2 = ex_doc_eletronico->msgv2.
            msg_var3 = ex_doc_eletronico->msgv3.
            msg_var4 = ex_doc_eletronico->msgv4.
            CALL FUNCTION 'MESSAGE_PREPARE'
              EXPORTING
                language = 'P'
                msg_id   = msg_id
                msg_no   = msg_no
                msg_var1 = msg_var1
                msg_var2 = msg_var2
                msg_var3 = msg_var3
                msg_var4 = msg_var4
              IMPORTING
                msg_text = wmessage.

            wa_saida-message = wmessage.
            MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING message.
        ENDTRY.
      ENDIF.
      IF ( ( wa_saida-cancel = 'X' OR e_info_doc_eletronico-cancel = 'X' ) AND wa_saida-nfenum IS NOT INITIAL ) OR  " Cancelado SEFAZ
         ( ( wa_saida-cancel IS INITIAL ) AND wa_saida-nfenum IS INITIAL ) OR
          ( wa_saida-docsta = 2 OR wa_saida-docsta = 3 ).  " Não enviado SEFAZ
        CALL FUNCTION 'ZNFW_ESTORNA_SEQ_LCTO'
          EXPORTING
            i_seq_lcto = wa_saida-seq_lcto
            i_estorno  = 'X'
          TABLES
            t_docs     = tl_docs.     " Estrutura de funcao de estorno de documentos
      ELSE.
        wa_saida-message = 'Nota já cancelada na SEFAZ/SAP'.
        MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING message.
      ENDIF.
    ELSEIF wa_saida-docnum IS INITIAL.
*-CS2021000723 - #74615 - 19.04.2022 - JT - inicio
      MESSAGE i024(sd) WITH 'Você será direcionado para a transação ZNFW0002,'
                            ' e deve fazer o cancelamento nela.'.

      SET PARAMETER ID 'SEQ' FIELD  wa_saida-seq_lcto.
      CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

*     wa_saida-message = 'Documento fiscal não gerado!'.
*     MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING message.
*-CS2021000723 - #74615 - 19.04.2022 - JT - fim
    ELSEIF wa_saida-ebeln IS NOT INITIAL.
      wa_saida-message = 'Elimine o pedido de compra associado!'.
      MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING message.
    ELSEIF wa_saida-docs_estornados = 'X'.
      wa_saida-message = 'Documento já estornado!'.
      MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING message.
    ELSEIF vnfnum_flag =  wa_saida-nfnum_flag.
      wa_saida-message = 'Aguarde retorno SEFAZ'.
      MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING message.
    ELSEIF wa_saida-nfenum IS INITIAL.
      wa_saida-message = 'Nota não autorizada. Estorno SEFAZ impossivel'.
      MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING message.
    ELSEIF wa_saida-cancel EQ 'X'.
      wa_saida-message = 'Nota já cancelada na SEFAZ'.
      MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING message.
    ENDIF.
  ENDLOOP.
  PERFORM z_busca_dados_atualizado.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_GERAR_DANFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_gerar_danfe .
  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  IF tg_selectedrow[] IS INITIAL.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'E'.
    EXIT.
  ENDIF.

  LOOP AT tg_selectedrow INTO wg_selectedrow.

    READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.
    CALL FUNCTION 'Z_SD_PRINT_NFE_CTE'
      EXPORTING
        doc_numero     = wa_saida-docnum
      EXCEPTIONS
        nao_localizado = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM f_define_origem_destino USING p_kna1  TYPE kna1
                                   p_lfa1  TYPE lfa1
                                   p_t001w TYPE t001w
                                   p_1baa  TYPE j_1baa
                                   wa_zfiwrt0001 TYPE zfiwrt0001
                          CHANGING p_indcoper
                                   p_texto_fiscal.

  IF wa_zfiwrt0001-parvw EQ 'AG'.
    IF p_kna1-regio EQ p_t001w-regio.
      p_indcoper = 'D'.
      p_texto_fiscal = 'Dentro do Estado'.
    ELSE.
      p_indcoper = 'F'.
      p_texto_fiscal = 'Fora do Estado'.
    ENDIF.
    IF p_1baa-direct EQ 1.
      MOVE: p_kna1-regio TO wg_shipfrom.
    ELSE.
      MOVE: p_kna1-regio TO wg_shipto.
    ENDIF.
  ELSEIF wa_zfiwrt0001-parvw EQ 'BR'
     OR  wa_zfiwrt0001-parvw EQ 'LF'.
    IF p_lfa1-regio EQ p_t001w-regio.
      p_indcoper = 'D'.
      p_texto_fiscal = 'Dentro do Estado'.
    ELSE.
      p_indcoper = 'F'.
      p_texto_fiscal = 'Fora do Estado'.
    ENDIF.

    IF p_1baa-direct EQ 1.
      MOVE: p_lfa1-regio TO wg_shipfrom.
    ELSE.
      MOVE: p_lfa1-regio TO wg_shipto.
    ENDIF.
  ENDIF.

  IF p_1baa-direct EQ 1.
    MOVE: p_t001w-regio TO wg_shipto.
  ELSE.
    MOVE: p_t001w-regio TO wg_shipfrom.
  ENDIF.

ENDFORM.
