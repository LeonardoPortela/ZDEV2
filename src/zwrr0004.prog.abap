*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 18/08/2010                                              &*
*& Descrição: Cockipt - Gestão de Emissão de Nota Fiscal              &*
*& Transação: ZNFW0003                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK917576   03.08.2010                            &*
*& Marcos Faneli   DEVK937089   29.04.2014
*&--------------------------------------------------------------------&*

REPORT  zwrr0004.
INCLUDE <icon>.
TYPE-POOLS: slis, kkblo.
TYPES: BEGIN OF ty_monitor,
         mark,
         status(20),
         status_proc         TYPE zfiwrt0008-status,
         operacao            TYPE zfiwrt0008-operacao,
         seq_lcto            TYPE zfiwrt0008-seq_lcto,
         bukrs               TYPE zfiwrt0008-bukrs,
         branch              TYPE zfiwrt0008-branch,
         parvw               TYPE zfiwrt0008-parvw,
         parid               TYPE zfiwrt0008-parid,
         nome1               TYPE kna1-name1,
         nftype              TYPE zfiwrt0008-nftype,
         netwr               TYPE zfiwrt0009-netwr,
         descricao           TYPE zfiwrt0001-descricao,
         usnam               TYPE zfiwrt0008-usnam,
         dt_criacao          TYPE zfiwrt0008-dt_criacao,
         hr_criacao          TYPE zfiwrt0008-hr_criacao,
         nivel_aprov         TYPE zfiwrt0007-nivel_aprov,
         aprov               TYPE zfiwrt0007-usnam,
         budat               TYPE zfiwrt0008-budat,
         bldat               TYPE zfiwrt0008-bldat,
         docnum_flag(20),
         docnum(10),
         nfnum_flag(20),
         nfnum(10),
         doc_contab_flag(20),
         doc_contab(10),
         belnr(10),
         belnr_flag(20),
         vbeln_r(10),
         vbeln_flag(20),
         doc_mat_flag(20),
         doc_mat(10),
         doc_transf_flag(20),
         doc_transf(10),
         disp_cct            TYPE c LENGTH 4,
         model               TYPE j_1baa-model,
         form                TYPE j_1baa-form.
*        style type lvc_t_styl,
TYPES: color        TYPE   kkblo_specialcol OCCURS 0.
TYPES: END OF ty_monitor,

BEGIN OF ty_itens,
  itmnum       TYPE zfiwrt0009-itmnum,
  matnr        TYPE zfiwrt0009-matnr,
  maktx        TYPE makt-maktx,
  charg        TYPE zfiwrt0009-charg,
  cfop         TYPE zfiwrt0009-cfop,
  menge        TYPE zfiwrt0009-menge,
  netpr        TYPE zfiwrt0009-netpr,
  netwr        TYPE zfiwrt0009-netwr,
  itmtyp       TYPE zfiwrt0009-itmtyp,
  bwkey        TYPE zfiwrt0009-bwkey,
  anln1        TYPE zfiwrt0009-anln1,
  anln2        TYPE zfiwrt0009-anln2,
  belnr_imb    TYPE zfiwrt0009-belnr_imb,
  vbeln_r      TYPE zfiwrt0009-vbeln_r,
  trans_status TYPE zfiwrt0009-trans_status,
END OF ty_itens,

BEGIN OF ty_makt,
  matnr TYPE makt-matnr,
  maktx TYPE makt-maktx,
END OF ty_makt.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.
*Class definition for AUTO-REFRESH
CLASS lcl_timer DEFINITION DEFERRED.
*Class definition for ALV toolbar
CLASS:      lcl_alv_toolbar   DEFINITION DEFERRED.
DATA: g_container          TYPE scrfname VALUE 'CC_GRID1',
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      grid1                TYPE REF TO cl_gui_alv_grid,
      grid2                TYPE REF TO cl_gui_alv_grid,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,       "splitter conteiner 1
      container_2          TYPE REF TO cl_gui_container,       "splitter conteiner 2
      splitter             TYPE REF TO cl_gui_splitter_container,
      ob_recev             TYPE REF TO lcl_timer,
      ob_timer             TYPE REF TO cl_gui_timer,
      wg_ativo.
*      OB_INDICATOR         TYPE REF TO CL_RSDMDD_PROGRESS_INDICATOR.
*Declaration for toolbar buttons
DATA : ty_toolbar TYPE stb_button.
** Criação de tabela dinamica
DATA: t_fieldcatalog TYPE lvc_t_fcat,
      w_fieldcatalog TYPE lvc_s_fcat,
      wa_layout      TYPE lvc_s_layo,
      wa_stable      TYPE lvc_s_stbl,
      estrutura      TYPE TABLE OF ty_estrutura,
      wa_estrutura   TYPE ty_estrutura,
      wg_indic_cont  TYPE sy-tabix.
*      style type lvc_t_styl with header line,
*      wa_style            type lvc_s_styl.

CONSTANTS: c_x              TYPE c VALUE 'X',
           c_a              TYPE c VALUE 'A',
           c_e              TYPE c VALUE 'E',
           c_r              TYPE c VALUE 'R',
           c_p              TYPE c VALUE 'P',
           c_br(2)          TYPE c VALUE 'BR',
           c_lf(2)          TYPE c VALUE 'LF',
           c_55(2)          TYPE c VALUE '55',
           c_57(2)          TYPE c VALUE '57',
           c_ag(2)          TYPE c VALUE 'AG',
           c_exit(4)        TYPE c VALUE 'EXIT',
           c_back(4)        TYPE c VALUE 'BACK',
           c_red(4)         TYPE c VALUE '@0A@',
           c_yellow(4)      TYPE c VALUE '@09@',
           c_green(4)       TYPE c VALUE '@08@',
           c_aguard(4)      TYPE c VALUE '@9R@',
           c_proces(6)      TYPE c VALUE 'PROCES',
           c_cancel(6)      TYPE c VALUE 'CANCEL',
           c_atuali(6)      TYPE c VALUE 'ATUALI',
           c_search(6)      TYPE c VALUE 'SEARCH',

           c_remov_cct      TYPE c VALUE 'REMOV_CCT' LENGTH 20,
           c_disp_cct       TYPE c VALUE 'DISP_CCT'  LENGTH 20,

           c_reprov(6)      TYPE c VALUE 'REPROV',
           c_estorna(7)     TYPE c VALUE 'ESTORNA',
           c_clos_itens(10) TYPE c VALUE 'CLOS_ITENS'.


DATA: tg_monitor  TYPE TABLE OF ty_monitor WITH HEADER LINE,
      tg_0007     TYPE TABLE OF zfiwrt0007 WITH HEADER LINE,
      tg_0008     TYPE TABLE OF zfiwrt0008 WITH HEADER LINE,
      tg_0009     TYPE TABLE OF zfiwrt0009 WITH HEADER LINE,
      tg_0011     TYPE TABLE OF zfiwrt0011 WITH HEADER LINE,
      tg_0012     TYPE TABLE OF zfiwrt0012 WITH HEADER LINE,
      tg_1000     TYPE TABLE OF zfiwrt1000 WITH HEADER LINE,
      tg_zib_chv  TYPE TABLE OF zib_contabil_chv WITH HEADER LINE,
      tg_zib_err  TYPE TABLE OF zib_contabil_err WITH HEADER LINE,
      tg_zib_cont TYPE TABLE OF zib_contabil WITH HEADER LINE,
      tg_itens    TYPE TABLE OF ty_itens,
      wg_itens    TYPE ty_itens,
      tg_makt     TYPE TABLE OF ty_makt,
      wg_makt     TYPE ty_makt.
DATA: tl_bdc           TYPE TABLE OF bdcdata,
      wl_bdc           TYPE bdcdata,
      g_init_once,
      g_ref_from_timer.

DATA: ok_code             TYPE sy-ucomm,
      p_seq_lcto          TYPE zfiwrt0008-seq_lcto,
      p_usuario           TYPE zfiwrt0007-usnam,
      p_bukrs             TYPE zfiwrt0008-bukrs,
      p_branch            TYPE zfiwrt0008-branch,
      p_parvw             TYPE zfiwrt0008-parvw,
      p_parid             TYPE zfiwrt0008-parid,
      p_opera             TYPE  zfiwrt0008-operacao,
      p_bldat             TYPE bsad-bldat,
      p_budat             TYPE bsad-budat,
      p_disp_cct          TYPE  zfiwrt0001-disp_nf_cct,

      wg_desc_branch(30),
      wg_desc_parvw(30),
      wg_desc_parid(30),
      wg_desc_bukrs(30),
      wg_desc_usuario(30),
      wg_msg(100).

DATA: zcl_cct_control_nf TYPE REF TO zcl_cct_control_nf.

RANGES: rg_erdat FOR zfiwrt0008-dt_criacao,
        rg_seq_lcto_ext FOR zfiwrt0008-seq_lcto.

CALL SCREEN 100.
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      on_hotsopt_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no,

      on_hotsopt_clicki FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_timer DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_finished FOR EVENT finished OF cl_gui_timer.
ENDCLASS.                    "lcl_receiver DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD on_double_click.
    DATA: tl_function TYPE ui_functions,
          wl_function LIKE LINE OF tl_function.

    DATA: wl_result TYPE i.
    IF e_row GT 0.
      READ TABLE tg_monitor INTO tg_monitor INDEX e_row.

      REFRESH: tg_itens.

      LOOP AT tg_0009 INTO tg_0009
         WHERE seq_lcto EQ tg_monitor-seq_lcto.

        READ TABLE tg_makt INTO wg_makt
          WITH KEY matnr = tg_0009-matnr.

        MOVE: tg_0009-itmnum TO wg_itens-itmnum,
              tg_0009-matnr  TO wg_itens-matnr,
              wg_makt-maktx  TO wg_itens-maktx,
              tg_0009-charg  TO wg_itens-charg,
              tg_0009-cfop   TO wg_itens-cfop,
              tg_0009-menge  TO wg_itens-menge,
              tg_0009-netpr  TO wg_itens-netpr,
              tg_0009-netwr  TO wg_itens-netwr,
              tg_0009-itmtyp TO wg_itens-itmtyp ,
              tg_0009-bwkey  TO wg_itens-bwkey,
              tg_0009-anln1  TO wg_itens-anln1,
              tg_0009-anln2  TO wg_itens-anln2,
              tg_0009-belnr_imb  TO wg_itens-belnr_imb,
              tg_0009-vbeln_r  TO wg_itens-vbeln_r,
              tg_0009-trans_status  TO wg_itens-trans_status.

        APPEND wg_itens TO tg_itens.
        CLEAR: wg_itens, wg_makt.
      ENDLOOP.

      CALL METHOD splitter->set_row_height
        EXPORTING
          id     = 1
          height = 50.

      CALL METHOD splitter->set_row_sash
        EXPORTING
          id    = 1
          type  = 0
          value = 1.

      IF container_2 IS  INITIAL.
        CALL METHOD splitter->get_container
          EXPORTING
            row       = 2
            column    = 1
          RECEIVING
            container = container_2.

        CREATE OBJECT grid2
          EXPORTING
            i_parent = container_2.

        PERFORM montar_layout_grid2.
        wa_layout-sel_mode = space.
        wa_layout-box_fname = space.
        CALL METHOD grid2->set_table_for_first_display
          EXPORTING
            it_toolbar_excluding = tl_function
            is_layout            = wa_layout
          CHANGING
            it_fieldcatalog      = t_fieldcatalog[]
            it_outtab            = tg_itens[].

        SET HANDLER:
                 lcl_event_handler=>on_hotsopt_clicki FOR grid2.

      ELSE.
        wa_stable-col = c_x.
        CALL METHOD grid2->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      ENDIF.
    ENDIF.
  ENDMETHOD.              "ON_DOUBLE_CLICK
  METHOD on_hotsopt_clicki.
    DATA: tl_1000 TYPE TABLE OF zfiwrt1000,
          vfield  TYPE zfiwrt1000-field.
    REFRESH: tl_1000.

    CASE e_column_id.
      WHEN 'BELNR_IMB'.
        READ TABLE tg_itens INTO wg_itens INDEX e_row_id-index.
        vfield = wg_itens-itmnum.
        CONDENSE vfield NO-GAPS.
        CONCATENATE 'BELNR_IMB' vfield INTO vfield.
        LOOP AT tg_1000 INTO tg_1000
           WHERE field    EQ vfield
             AND seq_lcto EQ tg_monitor-seq_lcto.

          APPEND tg_1000 TO tl_1000.
          CLEAR: tg_1000.
        ENDLOOP.
        IF tl_1000[] IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES tl_1000.
        ELSE.
          READ TABLE tg_itens INTO wg_itens INDEX e_row_id-index.
          IF wg_itens-belnr_imb IS NOT INITIAL..
            SET PARAMETER ID 'BLN' FIELD wg_itens-belnr_imb.
            SET PARAMETER ID 'BUK' FIELD tg_monitor-bukrs.
            SET PARAMETER ID 'GJR' FIELD tg_monitor-budat(4).
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        ENDIF.
      WHEN 'TRANS_STATUS'.
        READ TABLE tg_itens INTO wg_itens INDEX e_row_id-index.
        vfield = wg_itens-itmnum.
        CONDENSE vfield NO-GAPS.
        CONCATENATE 'TRANSF' vfield INTO vfield.
        LOOP AT tg_1000 INTO tg_1000
           WHERE field    EQ vfield
             AND seq_lcto EQ tg_monitor-seq_lcto.

          APPEND tg_1000 TO tl_1000.
          CLEAR: tg_1000.
        ENDLOOP.
        IF tl_1000[] IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES tl_1000.
        ENDIF.
    ENDCASE.
  ENDMETHOD.
  METHOD on_hotsopt_click.
    DATA: opt            TYPE ctu_params,
          tl_1000        TYPE TABLE OF zfiwrt1000,
          vl_nfobjn      TYPE j_1binterf-nfobjn,
          vl_docnum      TYPE j_1bnfdoc-docnum,
          rspar_tab      TYPE TABLE OF rsparams,
          rspar_line     LIKE LINE OF rspar_tab,
          wl_obj_key     TYPE zib_contabil_chv-obj_key,
          wl_1000        TYPE zfiwrt1000,
          wl_zib_err_aux TYPE zib_contabil_err.

    REFRESH: tl_bdc, tl_1000, rspar_tab.

    CASE e_column_id.
      WHEN 'SEQ_LCTO'.
        READ TABLE tg_monitor INTO tg_monitor INDEX e_row_id-index.

*        PERFORM F_PREENCHER_DYNPRO USING:
*        'X' 'ZWRR0002'             '0100',
*        ' ' 'P_SEQ_LCTO'           TG_MONITOR-SEQ_LCTO,
*        ' ' 'BDC_OKCODE'           'SEARCH'.
*
*        OPT-DISMODE = 'E'.
*        OPT-DEFSIZE = ' '.
*
*        CALL TRANSACTION 'ZNFW0002' USING TL_BDC OPTIONS FROM OPT.
        SET PARAMETER ID 'SEQ' FIELD  tg_monitor-seq_lcto.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.
      WHEN  'OPERACAO'.
        READ TABLE tg_monitor INTO tg_monitor INDEX e_row_id-index.

        PERFORM f_preencher_dynpro USING:
        'X' 'ZWRR0001'             '0100',
        ' ' 'ZFIWRT0001-OPERACAO'  tg_monitor-operacao,
        ' ' 'BDC_OKCODE'           'ATUALI'.

        opt-dismode = 'E'.
        opt-defsize = ' '.

        CALL TRANSACTION 'ZNFW0001' USING tl_bdc OPTIONS FROM opt.
      WHEN 'DOCNUM_FLAG'.
        READ TABLE tg_monitor INTO tg_monitor INDEX e_row_id-index.

        LOOP AT tg_1000 INTO tg_1000
          WHERE field    EQ 'DOCNUM'
            AND seq_lcto EQ tg_monitor-seq_lcto.

          APPEND tg_1000 TO tl_1000.
          CLEAR: tg_1000.
        ENDLOOP.
        IF tl_1000[] IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES tl_1000.

        ELSEIF tg_monitor-docnum IS NOT INITIAL.
          vl_docnum = tg_monitor-docnum.

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
        READ TABLE tg_monitor INTO tg_monitor INDEX e_row_id-index.
        READ TABLE tg_0008 INTO tg_0008
          WITH KEY seq_lcto = tg_monitor-seq_lcto.
*        CLEAR: wl_obj_key.
*        CONCATENATE 'ZGF' tg_monitor-seq_lcto tg_monitor-budat(4) INTO wl_obj_key.
        LOOP AT tg_zib_err INTO tg_zib_err
           WHERE obj_key EQ tg_0008-obj_key
             AND type    EQ 'E'.

          MOVE: tg_zib_err-type    TO wl_1000-type,
                tg_zib_err-message TO wl_1000-mensagem.

          APPEND wl_1000 TO tl_1000.
          CLEAR: wl_1000.
        ENDLOOP.
        IF tl_1000[] IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES tl_1000.

        ELSEIF tg_monitor-doc_contab IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD tg_monitor-doc_contab.
          SET PARAMETER ID 'BUK' FIELD tg_monitor-bukrs.
          SET PARAMETER ID 'GJR' FIELD tg_monitor-budat(4).

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'DOC_TRANSF_FLAG'.
        READ TABLE tg_monitor INTO tg_monitor INDEX e_row_id-index.
        READ TABLE tg_0008 INTO tg_0008
          WITH KEY seq_lcto = tg_monitor-seq_lcto.

        CLEAR: wl_obj_key.
        CONCATENATE tg_0008-obj_key 'I' INTO wl_obj_key.

        CLEAR: wl_zib_err_aux.
        SELECT SINGLE *
          INTO wl_zib_err_aux
          FROM zib_contabil_err
         WHERE obj_key = wl_obj_key
           AND type    = 'E'.

        IF sy-subrc = 0.
          MOVE: wl_zib_err_aux-type    TO wl_1000-type,
                wl_zib_err_aux-message TO wl_1000-mensagem.

          APPEND wl_1000 TO tl_1000.
          CLEAR: wl_1000.
        ENDIF.
        IF tl_1000[] IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES tl_1000.

        ELSEIF tg_monitor-doc_transf IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD tg_monitor-doc_transf.
          SET PARAMETER ID 'BUK' FIELD tg_monitor-bukrs.
          SET PARAMETER ID 'GJR' FIELD tg_monitor-budat(4).

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'VBELN_FLAG'.
        LOOP AT tg_1000 INTO tg_1000
         WHERE field    EQ 'VBELN_R'
           AND seq_lcto EQ tg_monitor-seq_lcto.

          APPEND tg_1000 TO tl_1000.
          CLEAR: tg_1000.
        ENDLOOP.
        IF tl_1000[] IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES tl_1000.
        ELSE.
          READ TABLE tg_monitor INTO tg_monitor INDEX e_row_id-index.
          IF tg_monitor-vbeln_r IS NOT INITIAL.
            SELECT SINGLE * INTO @DATA(wa_delivery)
              FROM likp
             WHERE vbeln EQ @tg_monitor-vbeln_r.

            CASE wa_delivery-vbtyp.
              WHEN 'J'.
                SET PARAMETER ID 'VL'    FIELD tg_monitor-vbeln_r.
                CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
              WHEN '7'.
                SET PARAMETER ID 'VL'  FIELD tg_monitor-vbeln_r.
                SET PARAMETER ID 'VLM' FIELD tg_monitor-vbeln_r.
                CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.
            ENDCASE.
          ENDIF.
        ENDIF.
      WHEN 'BELNR_FLAG'.
        LOOP AT tg_1000 INTO tg_1000
         WHERE field    EQ 'IMOB'
           AND seq_lcto EQ tg_monitor-seq_lcto.

          APPEND tg_1000 TO tl_1000.
          CLEAR: tg_1000.
        ENDLOOP.
        IF tl_1000[] IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES tl_1000.
        ELSE.
          READ TABLE tg_monitor INTO tg_monitor INDEX e_row_id-index.
          IF tg_monitor-belnr IS NOT INITIAL..
            SET PARAMETER ID 'BLN' FIELD tg_monitor-belnr.
            SET PARAMETER ID 'BUK' FIELD tg_monitor-bukrs.
            SET PARAMETER ID 'GJR' FIELD tg_monitor-budat(4).
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        ENDIF.
      WHEN 'NFNUM_FLAG'.
        READ TABLE tg_monitor INTO tg_monitor INDEX e_row_id-index.
        LOOP AT tg_1000 INTO tg_1000
           WHERE field    EQ 'NFENUM'
             AND seq_lcto EQ tg_monitor-seq_lcto..

          APPEND tg_1000 TO tl_1000.
          CLEAR: tg_1000.
        ENDLOOP.
        IF tl_1000[] IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES tl_1000.

        ELSE.
          IF tg_monitor-docnum IS NOT INITIAL.
*          AND tg_monitor-nfnum IS INITIAL.
***            PROCESSA NFE
            IF tg_monitor-model EQ c_55
            AND tg_monitor-form IS NOT INITIAL.
              PERFORM f_preencher_dynpro USING:
                 'X' 'Z_1BNFE_MONITOR'       '1000',
                 ' ' 'DOCNUM-LOW'            tg_monitor-docnum,
                 ' ' 'USER-LOW'              space,
                 ' ' 'DATE0-LOW'             space,
                 ' ' 'BUKRS-LOW'             tg_monitor-bukrs,
                 ' ' 'BDC_OKCODE'            'ONLI'.

              opt-dismode = 'E'.
              opt-defsize = ' '.
              opt-racommit = 'X'.

              CALL TRANSACTION 'ZNFE' USING tl_bdc OPTIONS FROM opt.
              CALL METHOD ob_timer->run.
*              break-point.
*              rspar_line-selname = 'DOCNUM'.
*              rspar_line-kind    = 'S'.
*              rspar_line-sign    = 'I'.
*              rspar_line-option  = 'EQ'.
*              rspar_line-low     = tg_monitor-docnum.
*              APPEND rspar_line TO rspar_tab.
*
*              rspar_line-selname = 'USER'.
*              rspar_line-kind    = 'S'.
*              rspar_line-sign    = 'I'.
*              rspar_line-option  = 'EQ'.
*              rspar_line-low     = space.
*              APPEND rspar_line TO rspar_tab.
*
*              rspar_line-selname = 'BUKRS'.
*              rspar_line-kind    = 'S'.
*              rspar_line-sign    = 'I'.
*              rspar_line-option  = 'EQ'.
*              rspar_line-low     = tg_monitor-bukrs.
*              APPEND rspar_line TO rspar_tab.
*
*              EXPORT TCODE = 'TCODE' TO MEMORY.
*
*              SUBMIT z_1bnfe_monitor WITH SELECTION-TABLE rspar_tab.
**                                     USING SELECTION-SETS.

            ELSEIF tg_monitor-model EQ c_57
             AND tg_monitor-form IS NOT INITIAL.
              PERFORM f_preencher_dynpro USING:
                 'X' 'Z_1BNFE_MONITOR'       '1000',
                 ' ' 'DOCNUM-LOW'            tg_monitor-docnum,
                 ' ' 'USER-LOW'              space,
                 ' ' 'DATE0-LOW'             space,
                 ' ' 'BUKRS-LOW'             tg_monitor-bukrs,
                 ' ' 'BDC_OKCODE'            'ONLI'.

              opt-dismode = 'E'.
              opt-defsize = ' '.
              opt-racommit = 'X'.

              CALL TRANSACTION 'ZCTE' USING tl_bdc OPTIONS FROM opt.
              CALL METHOD ob_timer->run.
            ENDIF.
**          ELSEIF tg_monitor-docnum IS NOT INITIAL
**             AND tg_monitor-nfnum IS NOT INITIAL.
****          EXIBE NFE
**            vl_docnum = tg_monitor-docnum.
**
**            CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
**              EXPORTING
**                doc_number         = vl_docnum
**              IMPORTING
**                obj_number         = vl_nfobjn
**              EXCEPTIONS
**                document_not_found = 1
**                docum_lock         = 2
**                OTHERS             = 3.
**
**            CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
**              EXPORTING
**                obj_number         = vl_nfobjn
**              EXCEPTIONS
**                object_not_found   = 1
**                scr_ctrl_not_found = 2
**                OTHERS             = 3.
          ENDIF.
        ENDIF.
      WHEN 'DOC_MAT_FLAG'.
        READ TABLE tg_monitor INTO tg_monitor INDEX e_row_id-index.
        LOOP AT tg_1000 INTO tg_1000
           WHERE field    EQ 'MBLNR'
             AND seq_lcto EQ tg_monitor-seq_lcto..

          APPEND tg_1000 TO tl_1000.
          CLEAR: tg_1000.
        ENDLOOP.
        IF tl_1000[] IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES tl_1000.

        ELSEIF tg_monitor-doc_mat IS NOT INITIAL.

* ---> S4 Migration - 19/07/2023 - LO
*          SET PARAMETER ID 'MBN' FIELD tg_monitor-doc_mat.
*          SET PARAMETER ID 'GJR' FIELD tg_monitor-budat(4).
*          CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

          CALL FUNCTION 'MIGO_DIALOG'
            EXPORTING
              i_action            = 'A04'
              i_refdoc            = 'R02'
              i_notree            = 'X'
              i_no_auth_check     = ''
              i_skip_first_screen = 'X'
              i_deadend           = 'X'
              i_okcode            = 'OK_GO'
              i_mblnr             = tg_monitor-doc_mat
              i_mjahr             = tg_monitor-budat(4)
            EXCEPTIONS
              illegal_combination = 1
              OTHERS              = 2.
* <--- S4 Migration - 19/07/2023 - LO

        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "on_hotsopt_click
ENDCLASS.           "LCL_EVENT_HANDLER IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: wl_desactive.
*   Add customized toolbar buttons.
*    READ TABLE TG_FIELDS TRANSPORTING NO FIELDS
*      WITH KEY GROUP1 = 'GR1'.
*    IF SY-SUBRC IS INITIAL.
*      WL_DESACTIVE = SPACE.
*    ELSE.
*      WL_DESACTIVE = 1.
*    ENDIF.
    ty_toolbar-icon      =  icon_submit.
    ty_toolbar-function  =  c_proces.
    ty_toolbar-quickinfo = 'Processar Documentos'.
    ty_toolbar-text      = 'Processar Documentos'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      =  icon_te_deduction.
    ty_toolbar-function  =  c_estorna.
    ty_toolbar-quickinfo = 'Estornar Documentos'.
    ty_toolbar-text      = 'Estornar Documentos'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
*    ty_toolbar-icon      =  icon_booking_stop.
*    ty_toolbar-function  =  C_SEARCH.
*    ty_toolbar-quickinfo = 'Reprovar documentos'.
*    ty_toolbar-disabled  = wl_desactive.
*    ty_toolbar-butn_type = 0.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*    CLEAR ty_toolbar.
**
*    ty_toolbar-butn_type = 3.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*    CLEAR ty_toolbar.
**   variable for Toolbar Button
    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  c_clos_itens.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    IF p_disp_cct IS NOT INITIAL.
      ty_toolbar-icon      = icon_okay.
      ty_toolbar-function  = c_disp_cct.
      ty_toolbar-text      = 'Disponibilizar CCT'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      = icon_cancel.
      ty_toolbar-function  = c_remov_cct.
      ty_toolbar-text      = 'Remover CCT'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

**   Call reorganize method of toolbar manager to
**   display the toolbar
    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.
    DATA: vl_complemento TYPE zfiwrt0001-complemento,
          vl_aprova      TYPE zfiwrt0001-lm_aprova,
          tl_index_rows  TYPE lvc_t_row,
          wl_index_rows  TYPE lvc_s_row,
          wl_answer,
          wl_cont        TYPE sy-tabix,
          tl_docs        TYPE TABLE OF zfiwrs0003,
          v_msg(180),
          wa_zsdt0001    TYPE zsdt0001.
*
*    CALL METHOD grid1->refresh_table_display
*      EXPORTING
*        is_stable = wa_stable.
    REFRESH: tl_docs.
    CLEAR: wl_cont.
    SORT: tg_zib_cont BY obj_key.

    CASE e_ucomm.
      WHEN c_disp_cct.
        PERFORM f_disp_cct.
      WHEN c_remov_cct.
        PERFORM f_remover_cct.
      WHEN c_estorna.
        CALL METHOD grid1->get_selected_rows
          IMPORTING
            et_index_rows = tl_index_rows.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question  = TEXT-014
          IMPORTING
            answer         = wl_answer
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.

        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        CASE wl_answer.
          WHEN '2' OR
               'A'.
            EXIT.
        ENDCASE.

        LOOP AT tl_index_rows INTO wl_index_rows.
          READ TABLE tg_monitor INTO tg_monitor INDEX wl_index_rows-index.

          SELECT SINGLE *
            FROM zsdt0001
            INTO wa_zsdt0001
            WHERE seq_lcto = tg_monitor-seq_lcto.

          IF sy-subrc = 0.                                  "zles0115
            IF  NOT '11_12' CS wa_zsdt0001-st_proc. "Nota fiscal não cancelada
              MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Este lançamento é original da ZLES0115'
                                                     'faça o estorno pelo cockpit'.
              CONTINUE.
            ENDIF.
          ENDIF.
          IF tg_monitor-status NE c_aguard.
            CALL FUNCTION 'ZNFW_ESTORNA_SEQ_LCTO'
              EXPORTING
                i_seq_lcto = tg_monitor-seq_lcto
                i_estorno  = c_x
              TABLES
                t_docs     = tl_docs.


            ADD 1 TO wl_cont.
          ELSE.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Os documentos que estão aguardando aprovação, não'
                                                   'serão processados!'.
          ENDIF.
        ENDLOOP.
        IF wl_cont GT 0.
          IF wg_ativo IS INITIAL.
*           Step 5 : Start Timer
            CALL METHOD ob_timer->run.

*               STARTING NEW TASK 'IF' performing  .
            wg_ativo = c_x.
          ENDIF.
          MESSAGE s836(sd) WITH 'Os documentos foram marcados para estorno!'.
        ENDIF.
      WHEN c_proces.
*        IF P_BUDAT IS INITIAL
*        OR P_BLDAT IS INITIAL.
*          MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'É nescessario o preenchimento dos campos'
*                                '"Data do Documento" e "Data do Lançamento".'.
*        ELSE.
        CALL METHOD grid1->get_selected_rows
          IMPORTING
            et_index_rows = tl_index_rows.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question  = TEXT-014
          IMPORTING
            answer         = wl_answer
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.

        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        CASE wl_answer.
          WHEN '2' OR
               'A'.
            EXIT.
        ENDCASE.

        LOOP AT tl_index_rows INTO wl_index_rows.
          READ TABLE tg_monitor INTO tg_monitor INDEX wl_index_rows-index.

          IF tg_monitor-status(4) NE c_aguard.
            IF tg_monitor-status_proc NE c_p.

              CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
                EXPORTING
                  i_seq_lcto = tg_monitor-seq_lcto.
            ENDIF.

            ADD 1 TO wl_cont.
          ELSE.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Os documentos que estão aguardando aprovação, não'
                                                   'serão processados!'.
          ENDIF.
        ENDLOOP.

        IF wl_cont GT 0.
          IF wg_ativo IS INITIAL.
            CALL METHOD ob_timer->run.
            wg_ativo = 'X'.
          ENDIF.
          MESSAGE s836(sd) WITH 'Os documentos foram marcados para processamento!'.
        ENDIF.



*          CLEAR: VL_COMPLEMENTO, VL_APROVA .
*          SELECT SINGLE COMPLEMENTO LM_APROVA
*            FROM ZFIWRT0001 INTO ( VL_COMPLEMENTO, VL_APROVA )
*           WHERE OPERACAO = TG_MONITOR-OPERACAO.
*
*          SELECT SINGLE *
*              FROM ZFIWRT0008
*              INTO TG_0008
*               WHERE SEQ_LCTO EQ TG_MONITOR-SEQ_LCTO.
*          IF  VL_APROVA NE 'N'.
*            IF ( TG_MONITOR-BUDAT <> SY-DATUM ) AND ( VL_COMPLEMENTO NE 'S' ).
*              MESSAGE 'Data de Documento no passado, para prosseguir alterar para data do dia.' TYPE 'I'.
*              EXIT.
*            ENDIF.
*          ENDIF.
*
*          IF TG_MONITOR-STATUS(4) NE C_AGUARD.
*
*            IF TG_MONITOR-STATUS_PROC NE C_P.
**                PERFORM gera_fiscal USING tg_monitor.
*              PERFORM ALTERA_STATUS_PROC USING TG_MONITOR.
*            ENDIF.
**              IF tg_monitor-doc_contab NE icon_checked
**              AND tg_monitor-docnum EQ icon_checked.
***                PERFORM gera_contabil USING tg_monitor.
**              ENDIF.
**              IF tg_monitor-doc_mat NE icon_checked.
***                PERFORM gera_doc_material USING tg_monitor.
**              ENDIF.
*            ADD 1 TO WL_CONT.
*          ELSE.
*            MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Os documentos que estão aguardando aprovação, não'
*                                                   'serão processados!'.
*          ENDIF.
*        ENDLOOP.
*        IF WL_CONT GT 0.
*          IF WG_ATIVO IS INITIAL.
**           Step 5 : Start Timer
*            CALL METHOD OB_TIMER->RUN.
*
**               STARTING NEW TASK 'IF' performing  .
*            WG_ATIVO = C_X.
*          ENDIF.
*          MESSAGE S836(SD) WITH 'Os documentos foram marcados para processamento!'.
*        ENDIF.
*        ENDIF.
*      WHEN c_search.
*

*        CALL METHOD grid1->get_selected_rows
*          IMPORTING
*            et_index_rows = tl_index_rows.
*
*        LOOP AT tl_index_rows INTO wl_index_rows.
*          READ TABLE tg_monitor INTO tg_monitor INDEX wl_index_rows-index.
*
*          PERFORM modifica_status USING c_r.
*
*        ENDLOOP.
*        MESSAGE s836(sd) WITH 'O(s) documentos foi(rao) reprovado(s)'.
      WHEN c_clos_itens.
*        BREAK-POINT.
        CALL METHOD splitter->set_row_height
          EXPORTING
            id     = 1
            height = 100.

        CALL METHOD splitter->set_row_sash
          EXPORTING
            id    = 1
            type  = 0
            value = 0.
    ENDCASE.

  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_timer IMPLEMENTATION.
  METHOD handle_finished.
*    call function 'Z_WAIT_CLOKING'.
*    add 1 to wg_indic_cont.
*data: wl_text type string.
*wl_text = wg_indic_cont.
*    call method ob_indicator->DISPLAY_PROGRESS
*        exporting
*          I_value               = wg_indic_cont
*          I_ADDITIONAL_TEXT     = wl_text.

    PERFORM limpa_campos.
    PERFORM busca_descricoes.
    PERFORM busca_dados.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

*    MESSAGE s836(SD) WITH 'Report refreshed at' sy-uzeit.
    CALL METHOD ob_timer->run.
  ENDMETHOD.                    "handle_finished
ENDCLASS.                    "lcl_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  DATA: event       TYPE cntl_simple_event,
        events      TYPE cntl_simple_events,
        tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE.
  IF sy-calld = 'X' AND p_seq_lcto IS INITIAL.
    GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD p_seq_lcto.
"Melhoria movimentações de ativos imobilizado ZNFW - BG #155188 - INICIO
    PERFORM limpa_campos.
    PERFORM busca_descricoes.
    PERFORM busca_dados.
"Melhoria movimentações de ativos imobilizado ZNFW - BG #155188 - FIM
  ENDIF.
  IF g_init_once IS INITIAL.
    CLEAR: wg_ativo.
    p_bldat = sy-datum.
    p_budat = sy-datum.
    g_init_once = c_x.
  ENDIF.
  IF g_custom_container IS INITIAL.
    wa_layout-ctab_fname = 'COLOR'.
    wa_layout-zebra      = c_x.
*    WA_LAYOUT-NO_TOOLBAR = C_X.
    wa_layout-box_fname  = 'MARK'.
    wa_layout-sel_mode   = 'A'.
*    wa_layout-edit_mode  = c_x.
*    WA_LAYOUT-SGL_CLK_HD = C_X.
*    wa_layout-no_rowmark = c_x.
*    WA_LAYOUT-CWIDTH_OPT    = C_X.
    wa_stable-row        = c_x.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.

    CALL METHOD splitter->set_row_sash
      EXPORTING
        id    = 1
        type  = 0
        value = 0.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_1.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container_1.


    PERFORM montar_layout_grid1.
    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.

*      * Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR grid1.
    SET HANDLER obg_toolbar->handle_user_command FOR grid1.

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

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_monitor[].

    SET HANDLER:
                  lcl_event_handler=>on_double_click FOR grid1.
    SET HANDLER:
                  lcl_event_handler=>on_hotsopt_click FOR grid1.

*    step 1 : initialise time control
    CREATE OBJECT ob_timer
      EXPORTING
        parent = container_1.

* Step 2 : Initialise object to receive event
    CREATE OBJECT ob_recev.

* Step 3 : Couple event to method
    SET HANDLER ob_recev->handle_finished FOR ob_timer. "

* Step 4 : Set Interval in seconds
    ob_timer->interval = 10.
*call method cl_progress_indicator->CLASS_CONSTRUCTOR.
*create object ob_indicator
*  exporting
*    I_TEXT                  = 'teste'.
* call method CL_AKB_PROGRESS_INDICATOR=>ASYNCHRONOUS_RFC_CALL.

  ELSE.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'Z001'.
  SET TITLEBAR 'Z001'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN c_search.
      PERFORM limpa_campos.
      PERFORM busca_descricoes.
      PERFORM busca_dados.
    WHEN c_atuali.
      PERFORM busca_dados.
    WHEN c_back.
      LEAVE TO SCREEN 0.
    WHEN c_cancel.
      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 100.
    WHEN c_exit.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_GRID1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_grid1 .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 ' '            ' '             'TG_MONITOR' 'STATUS'            'Status'             '4'   ' ' ' ' ' ',
*        1 'ZFIWRT0008'   'OPERACAO'      'TG_MONITOR' 'OPERACAO'         ' '                  '4'   ' ' ' ' 'X',
        2 'ZFIWRT0008'   'SEQ_LCTO'      'TG_MONITOR' 'SEQ_LCTO'          'Seq.Lcto.'          '9'   ' ' ' ' 'X',
*        2 'ZFIWRT0008'   'BUKRS'         'TG_MONITOR' 'BUKRS'            ' '                  '7'   ' ' ' ' ' ',
        3 'ZFIWRT0008'   'BRANCH'        'TG_MONITOR' 'BRANCH'            ' '                  '5'   ' ' ' ' ' ',
        4 'ZFIWRT0008'   'PARVW'         'TG_MONITOR' 'PARVW'             ' '                  '8'   ' ' ' ' ' ',
        4 'ZFIWRT0008'   'PARID'         'TG_MONITOR' 'PARID'             ' '                  '5'   ' ' ' ' ' ',
        5 'KNA1'         'NOME1'         'TG_MONITOR' 'NOME1'             'Nome'               '20'  ' ' ' ' ' ',
*        6 'ZFIWRT0008'   'NFTYPE'        'TG_MONITOR' 'NFTYPE'           ' '                  '10'  ' ' ' ' ' ',
        6 'KONP'         'KBETR'         'TG_MONITOR' 'NETWR'             'Valor NF'           '10'  ' ' ' ' ' ',
       13 'ZFIWRT0008'   'USNAM'         'TG_MONITOR' 'USNAM'             ' '                  '10'  ' ' ' ' ' ',
       14 'ZFIWRT0008'   'DT_CRIACAO'    'TG_MONITOR' 'DT_CRIACAO'        'Data'               '10'  ' ' ' ' ' ',
       15 'ZFIWRT0008'   'HR_CRIACAO'    'TG_MONITOR' 'HR_CRIACAO'        'Hora'               '10'  ' ' ' ' ' ',
        7 ' '            ' '             'TG_MONITOR' 'NIVEL_APROV'       'Nivel Aprovador'    '10'  ' ' ' ' ' ',
        8 ' '            ' '             'TG_MONITOR' 'APROV'             'Aprovador'          '10'  ' ' ' ' ' ',
       10 ' '            ' '             'TG_MONITOR' 'DOCNUM_FLAG'       'Doc.Num.'           '15'  ' ' ' ' 'X',
       11 ' '            ' '             'TG_MONITOR' 'NFNUM_FLAG'        'Nro.Nf.'            '15'  ' ' ' ' 'X',
       12 ' '            ' '             'TG_MONITOR' 'DOC_CONTAB_FLAG'   'Doc.Contabil'       '15'  ' ' ' ' 'X',
       10 ' '            ' '             'TG_MONITOR' 'BELNR_FLAG'        'Doc Vd. Imob.'      '15'  ' ' ' ' 'X',
       10 ' '            ' '             'TG_MONITOR' 'VBELN_FLAG'        'Remessa'            '15'  ' ' ' ' 'X',
       10 ' '            ' '             'TG_MONITOR' 'DOC_TRANSF_FLAG'   'Doc.Transf.Imob.'   '16'  ' ' ' ' 'X',
       9  ' '            ' '             'TG_MONITOR' 'DOC_MAT_FLAG'      'Doc.Material'       '15'  ' ' ' ' 'X',
       16 ' '            ' '             'TG_MONITOR' 'DISP_CCT'          'Disp.CCT'           '09'   ' ' ' ' ' '.





ENDFORM.                    " MONTAR_LAYOUT_GRID1
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_hotspot).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
*  w_fieldcatalog-key_sel       = 'X'.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-hotspot     = p_hotspot.

  IF p_field EQ 'ESTORNO'.
*    W_FIELDCATALOG-CHECKBOX = C_X.
  ENDIF.

  CASE p_field.
    WHEN 'DISP_CCT'.
      w_fieldcatalog-just = 'C'.
  ENDCASE.


*  IF P_FIELD EQ 'DMBTR'.
*
*  ENDIF.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DESCRICOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_descricoes .
  DATA: wl_0001      TYPE zfiwrt0001,
        wl_t001      TYPE t001,
        wl_1bbranch  TYPE j_1bbrancht,
        wl_1bad      TYPE j_1bad,
        wl_kna1      TYPE kna1,
        wl_lfa1      TYPE lfa1,
        wl_1badt     TYPE j_1badt,
        wl_user_addr TYPE user_addr.

  SELECT SINGLE *
        FROM t001
        INTO wl_t001
         WHERE bukrs EQ p_bukrs.

  IF sy-subrc IS INITIAL.
    MOVE: wl_t001-butxt TO wg_desc_bukrs.
  ELSE.
    CLEAR : wg_desc_bukrs.
  ENDIF.

  SELECT SINGLE *
      FROM j_1bbrancht
      INTO wl_1bbranch
       WHERE bukrs  EQ p_bukrs
         AND branch EQ p_branch
         and language eq sy-langu.

  IF sy-subrc IS INITIAL.
    MOVE: wl_1bbranch-name TO wg_desc_branch.
  ELSE.
    CLEAR : wg_desc_branch.
  ENDIF.

  SELECT SINGLE *
  FROM j_1bad
  INTO wl_1bad
   WHERE parvw EQ p_parvw.

  IF sy-subrc IS INITIAL.
    SELECT SINGLE *
      FROM j_1badt
      INTO wl_1badt
       WHERE spras EQ sy-langu
         AND parvw EQ p_parvw.

    MOVE: wl_1badt-partxt TO wg_desc_parvw.
  ELSE.
    CLEAR : wg_desc_parvw.
  ENDIF.
  IF p_parvw EQ c_ag.
    SELECT SINGLE *
      FROM kna1
      INTO wl_kna1
       WHERE kunnr EQ p_parid.

    IF sy-subrc IS INITIAL.
      MOVE: wl_lfa1-name1 TO wg_desc_parid.
    ELSE.
      CLEAR : wg_desc_parid.
    ENDIF.

  ELSEIF p_parvw EQ c_lf
      OR   p_parvw EQ c_br.
    SELECT SINGLE *
            FROM lfa1
            INTO wl_lfa1
             WHERE lifnr EQ p_parid.
    IF sy-subrc IS INITIAL.
      MOVE: wl_lfa1-name1 TO wg_desc_parid.
    ELSE.
      CLEAR : wg_desc_parid.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
    FROM user_addr
    INTO wl_user_addr
     WHERE bname EQ p_usuario.

  IF sy-subrc IS INITIAL.
    CONCATENATE wl_user_addr-name_first wl_user_addr-name_last INTO wg_desc_usuario SEPARATED BY
      space.
  ELSE.
    CLEAR: wg_desc_usuario.
  ENDIF.
ENDFORM.                    " BUSCA_DESCRICOES
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados .
  DATA: tl_0001      TYPE TABLE OF zfiwrt0001 WITH HEADER LINE,
        tl_0007      TYPE TABLE OF zfiwrt0007 WITH HEADER LINE,
        tl_0007_aux  TYPE TABLE OF zfiwrt0007,
        tl_0008      TYPE TABLE OF zfiwrt0008 WITH HEADER LINE,
        tl_0009      TYPE TABLE OF zfiwrt0009 WITH HEADER LINE,
        tl_0014      TYPE TABLE OF zfiwrt0014 WITH HEADER LINE,
        tl_0020      TYPE TABLE OF zfiwrt0020 WITH HEADER LINE,
        tl_bnfdoc    TYPE TABLE OF j_1bnfdoc WITH HEADER LINE,
        tl_kna1      TYPE TABLE OF kna1      WITH HEADER LINE,
        tl_lfa1      TYPE TABLE OF lfa1      WITH HEADER LINE,
        tl_baa       TYPE TABLE OF j_1baa    WITH HEADER LINE,
        tl_0011      TYPE TABLE OF zfiwrt0011 WITH HEADER LINE,
        tl_0012      TYPE TABLE OF zfiwrt0012 WITH HEADER LINE,
        tl_active    TYPE TABLE OF j_1bnfe_active WITH HEADER LINE,
        tl_zib_chv   TYPE TABLE OF zib_contabil_chv WITH HEADER LINE,
        tl_docest    TYPE TABLE OF zfiwrs0003 WITH HEADER LINE,
        tl_zlest0142 TYPE TABLE OF zlest0142 WITH HEADER LINE,
*        tl_0011 TYPE TABLE OF ZFIWRT0011 WITH HEADER LINE,
        wl_cont      TYPE sy-tabix.

  DATA: BEGIN OF tl_obj_key OCCURS 0,
          seq_lcto TYPE zfiwrt0008-seq_lcto,
          obj_key  TYPE zib_contabil_chv-obj_key,
        END OF tl_obj_key.

  RANGES : rl_seq_lcto FOR zfiwrt0008-seq_lcto,
           rl_bukrs    FOR zfiwrt0008-bukrs,
           rl_branch   FOR zfiwrt0008-branch,
           rl_parvw    FOR zfiwrt0008-parvw,
           rl_opera    FOR zfiwrt0008-operacao,
           rl_parid    FOR zfiwrt0008-parid,
           rl_usuario  FOR zfiwrt0008-usnam.

  REFRESH: rl_seq_lcto, rl_bukrs, rl_branch, rl_parvw, rl_parid, rl_usuario,
           rg_erdat.

  CLEAR: rl_seq_lcto, rl_bukrs, rl_branch, rl_parvw, rl_parid, rl_usuario.

  IF rg_seq_lcto_ext[] IS NOT INITIAL.
    LOOP AT rg_seq_lcto_ext.
      CLEAR: rl_seq_lcto.
      MOVE-CORRESPONDING rg_seq_lcto_ext TO rl_seq_lcto.
      APPEND rl_seq_lcto.
    ENDLOOP.
    ADD 1 TO wl_cont.
  ELSE.
    IF p_seq_lcto IS NOT INITIAL.
      rl_seq_lcto-sign = 'I'.
      rl_seq_lcto-option = 'EQ'.
      rl_seq_lcto-low = p_seq_lcto.
      APPEND rl_seq_lcto.
      ADD 1 TO wl_cont.
    ENDIF.
  ENDIF.

  IF p_bukrs IS NOT INITIAL.
    rl_bukrs-sign = 'I'.
    rl_bukrs-option = 'EQ'.
    rl_bukrs-low = p_bukrs.
    APPEND rl_bukrs.
    ADD 1 TO wl_cont.
  ENDIF.

  IF p_branch IS NOT INITIAL.
    rl_branch-sign = 'I'.
    rl_branch-option = 'EQ'.
    rl_branch-low = p_branch.
    APPEND rl_branch.
    ADD 1 TO wl_cont.
  ENDIF.

  IF p_parvw IS NOT INITIAL.
    rl_parvw-sign = 'I'.
    rl_parvw-option = 'EQ'.
    rl_parvw-low = p_parvw.
    APPEND rl_parvw.
    ADD 1 TO wl_cont.
  ENDIF.

  IF p_parid IS NOT INITIAL.
    rl_parid-sign = 'I'.
    rl_parid-option = 'EQ'.
    rl_parid-low = p_parid.
    APPEND rl_parid.
    ADD 1 TO wl_cont.
  ENDIF.

  IF p_usuario IS NOT INITIAL.
    rl_usuario-sign = 'I'.
    rl_usuario-option = 'EQ'.
    rl_usuario-low = p_usuario.
    APPEND rl_usuario.
    ADD 1 TO wl_cont.
  ENDIF.

  IF p_opera IS NOT INITIAL.
    rl_opera-sign = 'I'.
    rl_opera-option = 'EQ'.
    rl_opera-low = p_opera.
    APPEND rl_opera.
    ADD 1 TO wl_cont.
  ENDIF.

  IF NOT rg_erdat-high IS INITIAL.
    MOVE: 'I'                TO rg_erdat-sign,
          'BT'               TO rg_erdat-option.

    APPEND rg_erdat.
  ELSEIF NOT rg_erdat-low IS INITIAL.
    MOVE: 'I'                TO rg_erdat-sign,
          'EQ'               TO rg_erdat-option.
    APPEND rg_erdat.
  ENDIF.


  IF wl_cont LT 3.
    wg_msg = '@0S@ Você consegue melhorar a sua busca, preenchendo mais alguns campos!'.

  ELSE.
    CLEAR: wg_msg.

  ENDIF.

  IF rl_seq_lcto[] is INITIAL AND
     rl_bukrs[]    is INITIAL AND
     rl_branch[]   is INITIAL AND
     rl_parvw[]    is INITIAL AND
     rl_parid[]    is INITIAL AND
     rl_usuario[]  is INITIAL AND
     rg_erdat[]    is INITIAL AND
     rl_opera[]    is INITIAL.

    MESSAGE 'Nenhum filtro foi informado!' TYPE 'I'.
    RETURN.

  ENDIF.

** CABECARIO DA NOTA FISCAL
  SELECT *
    FROM zfiwrt0008
    INTO TABLE tl_0008
     WHERE seq_lcto IN rl_seq_lcto
*         AND operacao EQ tl_0007_aux-operacao
       AND bukrs    IN rl_bukrs
       AND branch   IN rl_branch
       AND parvw    IN rl_parvw
       AND parid    IN rl_parid
       AND usnam    IN rl_usuario
       AND budat    IN rg_erdat
       AND operacao IN rl_opera
       AND loekz    EQ space.

  IF sy-subrc IS INITIAL.
    LOOP AT tl_0008 WHERE obj_key IS NOT INITIAL..
      MOVE: tl_0008-seq_lcto TO tl_obj_key-seq_lcto,
            tl_0008-obj_key  TO tl_obj_key-obj_key.
*      CONCATENATE 'ZG' tl_0008-seq_lcto tl_0008-budat(4) INTO tl_obj_key-obj_key.

      APPEND tl_obj_key.
      CLEAR: tl_obj_key.
    ENDLOOP.
    tg_0008[] = tl_0008[].

    IF tl_obj_key[] IS NOT INITIAL.
      SELECT *
        FROM zib_contabil
        INTO TABLE tg_zib_cont
         FOR ALL ENTRIES IN tl_obj_key
         WHERE obj_key EQ tl_obj_key-obj_key.

      SELECT *
        FROM zib_contabil_chv
        INTO TABLE tl_zib_chv
        FOR ALL ENTRIES IN tl_obj_key
         WHERE obj_key EQ tl_obj_key-obj_key.

      SELECT *
        FROM zib_contabil_err
        INTO TABLE tg_zib_err
        FOR ALL ENTRIES IN tl_obj_key
         WHERE obj_key EQ tl_obj_key-obj_key.
    ENDIF.

    SELECT *
      FROM zfiwrt0007
      INTO TABLE tl_0007_aux
      FOR ALL ENTRIES IN tl_0008
       WHERE operacao EQ tl_0008-operacao.

    SELECT *
      FROM zfiwrt1000
      INTO TABLE tg_1000
       FOR ALL ENTRIES IN tg_0008
       WHERE seq_lcto EQ tg_0008-seq_lcto.

    SELECT *
      FROM kna1
      INTO TABLE tl_kna1
       FOR ALL ENTRIES IN tl_0008
       WHERE kunnr EQ tl_0008-parid.

    SELECT *
      FROM lfa1
      INTO TABLE tl_lfa1
       FOR ALL ENTRIES IN tl_0008
       WHERE lifnr EQ tl_0008-parid.

    SELECT *
      FROM zfiwrt0011
      INTO TABLE tg_0011
       FOR ALL ENTRIES IN tl_0008
       WHERE seq_lcto EQ tl_0008-seq_lcto.

    SELECT *
      FROM zfiwrt0011
      INTO TABLE tl_0011
       FOR ALL ENTRIES IN tl_0008
       WHERE seq_lcto EQ tl_0008-seq_lcto
         AND dmbtr    GT 0.

    SELECT *
      FROM zfiwrt0012
      INTO TABLE tl_0012
       FOR ALL ENTRIES IN tl_0008
       WHERE seq_lcto EQ tl_0008-seq_lcto.

    SELECT *
      FROM zfiwrt0020
      INTO TABLE tl_0020
       FOR ALL ENTRIES IN tl_0008
       WHERE seq_lcto EQ tl_0008-seq_lcto.

    SELECT *
      FROM j_1baa
      INTO TABLE tl_baa
       FOR ALL ENTRIES IN tl_0008
       WHERE nftype EQ tl_0008-nftype.

    SELECT *
      FROM j_1bnfdoc
      INTO TABLE tl_bnfdoc
       FOR ALL ENTRIES IN tl_0008
       WHERE docnum EQ tl_0008-docnum.

    IF tl_bnfdoc[] IS NOT INITIAL.
      SELECT *
        FROM j_1bnfe_active
        INTO TABLE tl_active
         FOR ALL ENTRIES IN tl_bnfdoc
          WHERE docnum EQ tl_bnfdoc-docnum.

    ENDIF.

    SELECT *
      FROM zfiwrt0012
      INTO TABLE tg_0012
       FOR ALL ENTRIES IN tl_0008
       WHERE seq_lcto EQ tl_0008-seq_lcto.

*    IF sy-subrc IS INITIAL.
**  iTENS DA NOTA FISCAL
    SELECT *
      FROM zfiwrt0009
      INTO TABLE tl_0009
       FOR ALL ENTRIES IN tl_0008
       WHERE seq_lcto EQ tl_0008-seq_lcto.

    tg_0009[] = tl_0009[].
    IF tg_0009[] IS NOT INITIAL.
      SELECT matnr maktx
        FROM makt
        INTO TABLE tg_makt
         FOR ALL ENTRIES IN tg_0009
         WHERE matnr EQ tg_0009-matnr.

*      ENDIF.
**  Parametrizacoes de operações
      SELECT *
        FROM zfiwrt0001
        INTO TABLE tl_0001
         FOR ALL ENTRIES IN tl_0008
          WHERE operacao EQ tl_0008-operacao.

*** NIVEIS NESCESSARIOS PARA SEREM APROVADOS
      SELECT *
        FROM zfiwrt0007
        INTO TABLE tl_0007
        FOR ALL ENTRIES IN tl_0008
         WHERE operacao EQ tl_0008-operacao.
      tg_0007[] = tl_0007[].

** LOG DE ACAO DE APROV/REPROV
      SELECT *
        FROM zfiwrt0014
        INTO TABLE tl_0014
        FOR ALL ENTRIES IN tl_0008
         WHERE seq_lcto EQ tl_0008-seq_lcto.

      LOOP AT tl_0008.
        MOVE: tl_0008-seq_lcto TO tl_docest-seq_lcto.

        APPEND tl_docest.
        CLEAR: tl_docest.
      ENDLOOP.
*** Busca documentos de estorno
      CALL FUNCTION 'ZNFW_ESTORNA_SEQ_LCTO'
*         EXPORTING
*           I_SEQ_LCTO       =
*           I_ESTORNO        =
        TABLES
          t_docs = tl_docest.

      SELECT *
        FROM zlest0142 INTO TABLE tl_zlest0142
         FOR ALL ENTRIES IN tl_0008
       WHERE seq_lcto EQ tl_0008-seq_lcto.

      PERFORM organiza_dados TABLES tl_0001
                                    tl_0008
                                    tl_0009
                                    tl_0007
                                    tl_0014
                                    tl_bnfdoc
                                    tl_kna1
                                    tl_lfa1
                                    tl_baa
                                    tl_0011
                                    tl_0012
                                    tl_active
                                    tl_zib_chv
                                    tl_docest
                                    tl_zlest0142.
*                                    tl_zib_err.

    ENDIF.
  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não foram encontradas solicitações de aprovação'
                                           'para seu usuário.'.
  ENDIF.

ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Module  SEARCH_PARID  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_parid INPUT.
  DATA: BEGIN OF dynpfields OCCURS 1.
          INCLUDE STRUCTURE dynpread.
  DATA: END OF dynpfields.

* Parameter für F4IF_FIELD_VALUE_REQUEST
  DATA: mc_obj LIKE help_info-mcobj.
  DATA: return_values LIKE ddshretval OCCURS 0 WITH HEADER LINE.
  DATA: da_display TYPE c.

  REFRESH:dynpfields.

  MOVE 'P_PARVW'  TO dynpfields-fieldname.
  APPEND dynpfields.
  MOVE 'P_BUKRS'  TO dynpfields-fieldname.
  APPEND dynpfields.
  MOVE 'P_PARID'  TO dynpfields-fieldname.
  APPEND dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname                   = sy-repid
      dynumb                   = sy-dynnr
      perform_input_conversion = 'X'
    TABLES
      dynpfields               = dynpfields
    EXCEPTIONS
      invalid_abapworkarea     = 1
      invalid_dynprofield      = 2
      invalid_dynproname       = 3
      invalid_dynpronummer     = 4
      invalid_request          = 5
      no_fielddescription      = 6
      invalid_parameter        = 7
      undefind_error           = 8
      OTHERS                   = 9.

  READ TABLE dynpfields
      WITH KEY fieldname = 'P_PARVW'.

  IF p_parvw EQ c_ag
  OR dynpfields-fieldvalue EQ c_ag.

    READ TABLE dynpfields
       WITH KEY fieldname = 'P_PARID'.
*   Matchcodeobjekt aufrufen
    mc_obj = 'C_KUNNR'.
    IF dynpfields-fieldinp EQ space.
      da_display = c_x. "CHARX.
    ENDIF.
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = space
        fieldname         = space
        searchhelp        = mc_obj
        dynprofield       = 'X'
        value             = dynpfields-fieldvalue
        display           = da_display
      TABLES
        return_tab        = return_values
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.
    IF sy-subrc = 0 AND
       dynpfields-fieldinp NE space.
      p_parid = return_values-fieldval.
    ENDIF.

  ELSEIF p_parvw EQ c_br
    OR   p_parvw EQ c_lf
    OR dynpfields-fieldvalue EQ c_br
    OR dynpfields-fieldvalue EQ c_lf.

    READ TABLE dynpfields
       WITH KEY fieldname = 'P_PARID'.
*   Matchcodeobjekt aufrufen
    mc_obj = 'KRED_C'.
    IF dynpfields-fieldinp EQ space.
      da_display = c_x. "CHARX.
    ENDIF.
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = space
        fieldname         = space
        searchhelp        = mc_obj
        dynprofield       = 'X'
        value             = dynpfields-fieldvalue
        display           = da_display
      TABLES
        return_tab        = return_values
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.
    IF sy-subrc = 0 AND
       dynpfields-fieldinp NE space.
      p_parid = return_values-fieldval.
    ENDIF.
  ENDIF.
ENDMODULE.                 " SEARCH_PARID  INPUT
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_0008  text
*      -->P_TL_0009  text
*      -->P_TL_O007  text
*      -->P_TL_0014  text
*----------------------------------------------------------------------*
FORM organiza_dados  TABLES   tl_0001    STRUCTURE zfiwrt0001
                              tl_0008    STRUCTURE zfiwrt0008
                              tl_0009    STRUCTURE zfiwrt0009
                              tl_0007    STRUCTURE zfiwrt0007
                              tl_0014    STRUCTURE zfiwrt0014
                              tl_bnfdoc  STRUCTURE j_1bnfdoc
                              tl_kna1    STRUCTURE kna1
                              tl_lfa1    STRUCTURE lfa1
                              tl_baa     STRUCTURE j_1baa
                              tl_0011    STRUCTURE zfiwrt0011
                              tl_0012    STRUCTURE zfiwrt0012
                              tl_active  STRUCTURE j_1bnfe_active
                              tl_zib_chv STRUCTURE zib_contabil_chv
                              tl_docest  STRUCTURE zfiwrs0003
                              tl_zles0142 STRUCTURE zlest0142.
*                              tl_zib_err STRUCTURE zib_contabil_err.

  DATA: BEGIN OF tl_0009_aux OCCURS 0,
          seq_lcto TYPE zfiwrt0009-seq_lcto,
          netwr    TYPE zfiwrt0009-netwr,
          vbeln_r  TYPE zfiwrt0009-vbeln_r,
        END OF tl_0009_aux.

  DATA: wl_color TYPE kkblo_specialcol,
        BEGIN OF tl_infogrid,
          color TYPE kkblo_specialcol OCCURS 1,
        END OF tl_infogrid.

  DATA: x_aguardando,
        x_jarespod,
        x_achou_aprov(1),
        wl_status(20),
        v_chave          TYPE c LENGTH 44,
        wl_obj_key       TYPE zib_contabil_chv-obj_key.

  DATA: vl_objkey_transf_imob TYPE zib_contabil_chv-obj_key,
        wl_zib_err_aux        TYPE zib_contabil_err,
        wl_zib_chv_aux        TYPE zib_contabil_chv.


  LOOP AT tl_0009.
    MOVE-CORRESPONDING: tl_0009 TO tl_0009_aux.
    COLLECT tl_0009_aux.
  ENDLOOP.

**> Deixa apenas a ultima ação do usuario aprovador
  SORT: tl_0014 BY seq_lcto nivel_aprov dzeile DESCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_0014 COMPARING seq_lcto nivel_aprov.
**<
  SORT: tl_0007 BY nivel_aprov,
        tl_zib_chv BY obj_key,
        tl_docest BY seq_lcto.

  LOOP AT tl_0008.    "header do documento

    "Caso houver reprovação Ignorar o Registro.
    READ TABLE tl_0014
      WITH KEY seq_lcto    = tl_0008-seq_lcto
               tipo        = c_r.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
    CLEAR  x_aguardando.

    IF tl_0008-status IS INITIAL.
**> Verificador se deve o registro deve ser exibido para aprovacao,
** pois o documento so pode ser exibido para aprovação caso o mesmo, nao tenha
** sido aprovado por um nivel a cima ou se ja processado (falta segunda parte)
      LOOP AT tl_0007   " Aprovadores por operação
         WHERE operacao EQ tl_0008-operacao.
        x_achou_aprov = 'N'.
        IF tl_0007-branch IS INITIAL.
          tl_0007-branch = tl_0008-branch.
        ENDIF.
        IF tl_0007-branch EQ tl_0008-branch.
          READ TABLE tl_0014
            WITH KEY seq_lcto    = tl_0008-seq_lcto
                     nivel_aprov = tl_0007-nivel_aprov
                     tipo        = c_a.
          IF sy-subrc = 0.
            x_achou_aprov = 'S'.
          ENDIF.

          MOVE: tl_0007-nivel_aprov TO tg_monitor-nivel_aprov,
               tl_0007-usnam       TO tg_monitor-aprov.

          READ TABLE tl_0001
               WITH KEY operacao = tl_0008-operacao.

          IF tl_0001-lm_aprova NE 'N' AND x_achou_aprov = 'N'.
            x_aguardando = c_x.
            EXIT.
          ENDIF.

        ENDIF.
      ENDLOOP.

      IF x_aguardando IS NOT INITIAL.
        CLEAR: x_aguardando.
*      wl_status = c_aguard.                                 "'@9R@'.
        CONCATENATE c_aguard 'Em Aprovação' INTO wl_status SEPARATED BY ' - '.
      ENDIF.
    ENDIF.

    READ TABLE tl_0001
      WITH KEY operacao = tl_0008-operacao.

    IF ( p_disp_cct IS NOT INITIAL ) AND ( sy-subrc NE 0 OR tl_0001-disp_nf_cct NE 'S' ).
      CONTINUE.
    ENDIF.

    READ TABLE tl_0009_aux
      WITH KEY seq_lcto = tl_0008-seq_lcto.

    CONCATENATE 'ZGF' tl_0008-seq_lcto tl_0008-budat(4) INTO wl_obj_key.

    READ TABLE tl_zib_chv
      WITH KEY obj_key = tl_0008-obj_key
                 BINARY SEARCH.

    IF wl_status IS INITIAL.
      IF tl_0008-docnum IS INITIAL
      AND tl_zib_chv-belnr IS INITIAL
      AND tl_0008-nfenum IS INITIAL
      AND tl_0008-mblnr IS INITIAL.
*      IF tl_0008-STATUS IS INITIAL.
        wl_status = c_red.                                  "'@0A@'.

      ELSEIF tl_0008-docnum IS NOT INITIAL
      OR tl_zib_chv-belnr IS NOT  INITIAL
      OR tl_0008-nfenum IS NOT INITIAL
      OR tl_0008-mblnr IS NOT INITIAL.
*      ELSEIF tl_0008-STATUS EQ C_A
*         OR  TL_0008-STATUS EQ C_E.
        wl_status = c_yellow.                               "'@09@'.

      ELSEIF tl_0008-status EQ 'P' "IS NOT INITIAL
      AND tl_zib_chv-belnr IS NOT  INITIAL
      AND tl_0008-nfenum IS NOT INITIAL.
***      AND tl_0008-mblnr IS NOT INITIAL.
*      ELSEIF tl_0008-STATUS EQ C_P
*         AND TL_.
        wl_status = c_green.                                "'@08@'.

      ENDIF.
    ENDIF.

    IF tl_0008-parvw EQ c_ag.
      READ TABLE tl_kna1
        WITH KEY kunnr = tl_0008-parid.

      MOVE: tl_kna1-name1 TO tg_monitor-nome1.

    ELSEIF tl_0008-parvw EQ c_lf
        OR tl_0008-parvw EQ c_br.
      READ TABLE tl_lfa1
        WITH KEY lifnr = tl_0008-parid.

      MOVE: tl_lfa1-name1 TO tg_monitor-nome1.

    ENDIF.

    READ TABLE tl_baa
      WITH KEY nftype = tl_0008-nftype.

*    IF TL_BAA-FORM IS NOT INITIAL.
    READ TABLE tl_bnfdoc
      WITH KEY docnum = tl_0008-docnum.
    IF sy-subrc IS INITIAL.
      READ TABLE tl_active
        WITH KEY docnum = tl_bnfdoc-docnum.

      MOVE: tl_bnfdoc-nfenum TO tl_0008-nfenum.
    ENDIF.
*    ENDIF.

    MOVE: wl_status         TO tg_monitor-status,
          tl_0008-operacao  TO tg_monitor-operacao,
          tl_0008-seq_lcto  TO tg_monitor-seq_lcto,
          tl_0008-status    TO tg_monitor-status_proc,
          tl_0008-bukrs     TO tg_monitor-bukrs,
          tl_0008-branch    TO tg_monitor-branch,
          tl_0008-parvw     TO tg_monitor-parvw,
          tl_0008-parid     TO tg_monitor-parid,
          tl_0008-nftype    TO tg_monitor-nftype,
          tl_0009_aux-netwr TO tg_monitor-netwr,
          tl_0001-descricao TO tg_monitor-descricao,
          tl_0008-usnam     TO tg_monitor-usnam,
          tl_0008-dt_criacao TO tg_monitor-dt_criacao,
          tl_0008-hr_criacao TO tg_monitor-hr_criacao,
          tl_0008-bldat      TO tg_monitor-bldat,
          tl_0008-budat      TO tg_monitor-budat,
          tl_baa-model       TO tg_monitor-model,
          tl_baa-form        TO tg_monitor-form.
*          sy-uname           TO tg_monitor-aprov.

    READ TABLE tg_1000
      WITH KEY seq_lcto = tl_0008-seq_lcto
               field    = 'VBELN_R'.
    IF sy-subrc IS INITIAL.
      tg_monitor-vbeln_flag = icon_status_critical.
    ELSE.
      IF tl_0009_aux-vbeln_r IS INITIAL.
        tg_monitor-vbeln_flag = icon_warning.
      ELSE.
        SHIFT tl_0009_aux-vbeln_r LEFT DELETING LEADING '0'.
        CONCATENATE  icon_checked tl_0009_aux-vbeln_r INTO tg_monitor-vbeln_flag SEPARATED BY ' - '.
        tg_monitor-vbeln_r = tl_0009_aux-vbeln_r.

      ENDIF.
    ENDIF.

    READ TABLE tg_1000
      WITH KEY seq_lcto = tl_0008-seq_lcto
               field    = 'DOCNUM'.
    IF sy-subrc IS INITIAL.
      tg_monitor-docnum_flag = icon_status_critical.
    ELSE.
      IF tl_0008-docnum IS INITIAL.
        tg_monitor-docnum_flag = icon_warning.
      ELSE.
        SHIFT tl_0008-docnum LEFT DELETING LEADING '0'.
        CONCATENATE  icon_checked tl_0008-docnum INTO tg_monitor-docnum_flag SEPARATED BY ' - '.
        tg_monitor-docnum = tl_0008-docnum.
*        tg_monitor-docnum = icon_checked.
      ENDIF.
    ENDIF.

    IF tl_0008-belnr_imb IS INITIAL OR tl_0008-belnr_imb = ''.
      tg_monitor-belnr_flag = icon_status_critical.
    ELSE.
      SHIFT tl_0008-belnr_imb LEFT DELETING LEADING '0'.
      CONCATENATE  icon_checked tl_0008-belnr_imb INTO tg_monitor-belnr_flag SEPARATED BY ' - '.
      tg_monitor-belnr_flag = tl_0008-belnr_imb.
      tg_monitor-belnr      = tl_0008-belnr_imb.
    ENDIF.

    READ TABLE tg_zib_err
     WITH KEY obj_key = tl_0008-obj_key
           type    = 'E'.
*           field    = 'BELNR'.
    IF sy-subrc IS INITIAL.
      tg_monitor-doc_contab_flag = icon_status_critical.

    ELSE.
      IF tl_zib_chv-belnr IS INITIAL.
        READ TABLE tl_0011
        WITH KEY seq_lcto = tl_0008-seq_lcto.
        IF sy-subrc IS INITIAL.
          tg_monitor-doc_contab_flag = icon_warning.
        ELSE.
          tg_monitor-doc_contab_flag =  icon_system_okay.
        ENDIF.
      ELSE.
        SHIFT tl_zib_chv-belnr LEFT DELETING LEADING '0'.
        CONCATENATE  icon_checked tl_zib_chv-belnr INTO tg_monitor-doc_contab_flag SEPARATED BY ' - '.
        tg_monitor-doc_contab = tl_zib_chv-belnr.
      ENDIF.
    ENDIF.

    "Documento Contabil. Imposto Saida Transferência Imobilizado.
    IF ( tl_0008-tp_mv_imob = 'T' ) AND ( tl_baa-direct  = 2 ) AND ( tl_0008-obj_key IS NOT INITIAL ).

      CLEAR: vl_objkey_transf_imob.
      CONCATENATE tl_0008-obj_key 'I' INTO vl_objkey_transf_imob.

      CLEAR: wl_zib_err_aux, wl_zib_chv_aux.
      SELECT SINGLE *
        INTO wl_zib_err_aux
        FROM zib_contabil_err
       WHERE obj_key = vl_objkey_transf_imob
         AND type    = 'E'.

      IF sy-subrc IS INITIAL.
        tg_monitor-doc_transf_flag = icon_status_critical.
      ELSE.
        SELECT SINGLE *
          INTO wl_zib_chv_aux
          FROM zib_contabil_chv
         WHERE obj_key = vl_objkey_transf_imob.

        IF wl_zib_chv_aux-belnr IS INITIAL.
          READ TABLE tl_0011
          WITH KEY seq_lcto = tl_0008-seq_lcto.
          IF sy-subrc IS INITIAL.
            tg_monitor-doc_transf_flag = icon_warning.
          ELSE.
            tg_monitor-doc_transf_flag = icon_system_okay.
          ENDIF.
        ELSE.
          SHIFT wl_zib_chv_aux-belnr LEFT DELETING LEADING '0'.
          CONCATENATE  icon_checked wl_zib_chv_aux-belnr INTO tg_monitor-doc_transf_flag SEPARATED BY ' - '.
          tg_monitor-doc_transf = wl_zib_chv_aux-belnr.
        ENDIF.
      ENDIF.

    ENDIF.
    "Fim Doc. Transf. Imob.

    IF tl_0008-nfenum IS INITIAL
    AND tl_0008-docnum IS INITIAL.
      tg_monitor-nfnum_flag = icon_warning.

    ELSEIF tl_0008-nfenum IS INITIAL
       AND tl_0008-docnum IS NOT INITIAL.

      IF tg_monitor-model EQ c_55.
        CONCATENATE icon_generate 'Gerar DANFE' INTO tg_monitor-nfnum_flag SEPARATED BY ' - '.
      ELSEIF tg_monitor-model EQ c_57.
        CONCATENATE icon_generate 'Gerar DACTE' INTO tg_monitor-nfnum_flag SEPARATED BY ' - '.
      ENDIF.

    ELSEIF tl_0008-nfenum IS NOT INITIAL.
      IF tl_baa-form IS NOT INITIAL.
        SHIFT tl_0008-nfenum LEFT DELETING LEADING '0'.
        IF tl_active-docsta EQ space.
          CONCATENATE  icon_activity tl_0008-nfenum INTO tg_monitor-nfnum_flag SEPARATED BY ' - '.
        ELSEIF tl_active-docsta EQ '1'.
          CONCATENATE  icon_complete tl_0008-nfenum INTO tg_monitor-nfnum_flag SEPARATED BY ' - '.
        ELSE.
          CONCATENATE  icon_status_critical tl_0008-nfenum INTO tg_monitor-nfnum_flag SEPARATED BY ' - '.
        ENDIF.
        tg_monitor-nfnum = tl_0008-nfenum.
      ELSE.
        IF tl_active-docsta EQ '1'
        AND tl_active-action_requ EQ '6'.
          CONCATENATE  icon_warning tl_0008-nfenum INTO tg_monitor-nfnum_flag SEPARATED BY ' - '.
        ELSEIF tl_active-docsta EQ '1'
           AND tl_active-action_requ EQ 'C'.
          CONCATENATE  icon_complete tl_0008-nfenum INTO tg_monitor-nfnum_flag SEPARATED BY ' - '.
        ELSEIF tl_active-docnum IS NOT INITIAL.
          CONCATENATE  icon_status_critical tl_0008-nfenum INTO tg_monitor-nfnum_flag SEPARATED BY ' - '.
        ELSE.
          CONCATENATE  icon_warning tl_0008-nfenum INTO tg_monitor-nfnum_flag SEPARATED BY ' - '.
        ENDIF.
        tg_monitor-nfnum = tl_0008-nfenum.
      ENDIF.
    ENDIF.

    READ TABLE tg_1000
      WITH KEY seq_lcto = tl_0008-seq_lcto
               field    = 'MBLNR'.
    IF sy-subrc IS INITIAL.
      tg_monitor-doc_mat_flag = icon_status_critical.

    ELSE.
      IF tl_0008-mblnr IS INITIAL.
        READ TABLE tl_0012
         WITH KEY seq_lcto = tl_0008-seq_lcto.
        IF sy-subrc IS INITIAL.
          tg_monitor-doc_mat_flag = icon_warning.
        ELSE.
          tg_monitor-doc_mat_flag = icon_system_okay.
        ENDIF.
      ELSE.
        SHIFT tl_0008-mblnr LEFT DELETING LEADING '0'.
        CONCATENATE  icon_checked tl_0008-mblnr INTO tg_monitor-doc_mat_flag SEPARATED BY ' - '.
        tg_monitor-doc_mat = tl_0008-mblnr.
*        tg_monitor-doc_mat = icon_checked.
      ENDIF.
    ENDIF.

    IF tg_monitor-status(4) NE c_aguard.
      IF tg_monitor-docnum_flag(4) EQ icon_warning
      AND tg_monitor-nfnum_flag(4) EQ icon_warning
      AND tl_0011 IS INITIAL
      AND tl_0012 IS INITIAL.
        tg_monitor-status = c_red.

      ENDIF.

      IF tg_monitor-docnum_flag(4) EQ icon_checked
      AND tg_monitor-nfnum_flag(4) EQ icon_complete.
        IF ( tl_0011 IS INITIAL
        OR tg_monitor-doc_contab_flag(4) EQ icon_checked )
        AND ( tl_0012 IS INITIAL
         OR tg_monitor-doc_mat_flag(4) EQ icon_checked ).

          tg_monitor-status = c_green.
        ENDIF.
      ENDIF.
    ENDIF.

***    Cor
    CLEAR: tl_docest, tg_monitor-color.
    REFRESH: tg_monitor-color.
    READ TABLE tl_docest
      WITH KEY seq_lcto = tl_0008-seq_lcto
                  BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      IF tg_monitor-vbeln_r IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = tg_monitor-vbeln_r
          IMPORTING
            output = tg_monitor-vbeln_r.
        SELECT SINGLE *
          FROM likp
          INTO @DATA(wlikp)
          WHERE vbeln = @tg_monitor-vbeln_r.
        IF sy-subrc NE 0.
          CLEAR: wl_color.
          wl_color-fieldname = 'VBELN_FLAG'.
          wl_color-color-col = 6.
          wl_color-color-inv = 6.
          APPEND wl_color TO tg_monitor-color.
        ENDIF.

      ENDIF.
**   Doc. de nota fiscal
      IF tl_docest-docnum_est IS NOT INITIAL
      AND tl_docest-docnum_est NE '0000000000'.
        CLEAR: wl_color.
        wl_color-fieldname = 'DOCNUM_FLAG'.
        wl_color-color-col = 6.
        wl_color-color-inv = 6.
        APPEND wl_color TO tg_monitor-color.
      ENDIF.
**    Doc. de material.
      IF tl_docest-mblnr_est IS NOT INITIAL
      AND tl_docest-mblnr_est NE '0000000000'.
        CLEAR: wl_color.
        wl_color-fieldname = 'DOC_MAT_FLAG'.
        wl_color-color-col = 6.
        wl_color-color-inv = 6.
        APPEND wl_color TO tg_monitor-color.
      ENDIF.
**    Doc.Contabíl
      IF tl_docest-belnr_est IS NOT INITIAL
      AND tl_docest-belnr_est NE '0000000000'.
        CLEAR: wl_color.
        wl_color-fieldname = 'DOC_CONTAB_FLAG'.
        wl_color-color-col = 6.
        wl_color-color-inv = 6.
        APPEND wl_color TO tg_monitor-color.
      ENDIF.

**    Doc.Contabíl Transf Imob
      IF tl_docest-tblnr_est IS NOT INITIAL
      AND tl_docest-tblnr_est NE '0000000000'.
        CLEAR: wl_color.
        wl_color-fieldname = 'DOC_TRANSF_FLAG'.
        wl_color-color-col = 6.
        wl_color-color-inv = 6.
        APPEND wl_color TO tg_monitor-color.
      ENDIF.

    ENDIF.
***

    READ TABLE tl_zles0142 WITH KEY seq_lcto = tg_monitor-seq_lcto.
    IF sy-subrc = 0.
      tg_monitor-disp_cct  = icon_okay.
    ELSE.
      tg_monitor-disp_cct  = icon_led_yellow.
    ENDIF.

    APPEND tg_monitor.
    CLEAR: x_aguardando, x_jarespod, tg_monitor, wl_status, tl_bnfdoc, tl_baa,
           tl_0011, tl_0012, tl_active, wl_obj_key, tl_zib_chv.
*    ENDIF.
  ENDLOOP.

ENDFORM.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_campos .

  REFRESH: tg_monitor, tg_0007, tg_0009, tg_0011, tg_0012, tg_1000, tg_itens, tg_makt,
           tg_zib_err, tg_zib_cont, tg_zib_chv.
  CLEAR: wg_desc_branch, wg_desc_parvw, wg_desc_parid, wg_desc_bukrs, wg_desc_usuario,
         wg_msg, wg_itens, wg_makt.
ENDFORM.                    " LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_GRID2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_grid2 .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'ZFIWRT0009'   'ITMNUM'        'TG_ITENS' 'ITMNUM'        ' '             '6'   ' ' ' ' ' ',
        1 'ZFIWRT0009'   'MATNR'         'TG_ITENS' 'MATNR'         ' '             '15'  ' ' ' ' ' ',
        2 'MAKT'         'MAKTX'         'TG_ITENS' 'MAKTX'         ' '             '25'  ' ' ' ' ' ',
        2 'ZFIWRT0009'   'CHARG'         'TG_ITENS' 'CHARG'         ' '             '10'  ' ' ' ' ' ',
        3 'ZFIWRT0009'   'CFOP'          'TG_ITENS' 'CFOP'          ' '             '8'   ' ' ' ' ' ',
        4 'ZFIWRT0009'   'BWKEY'         'TG_ITENS' 'BWKEY'         'Centro'        '8'   ' ' ' ' ' ',
        5 'ZFIWRT0009'   'ITMTYP'        'TG_ITENS' 'ITMTYP'        ' '             '4'   ' ' ' ' ' ',
        6 'ZFIWRT0009'   'ANLN1'         'TG_ITENS' 'ANLN1'         ' '             '8'  ' ' ' ' ' ',
        7 'ZFIWRT0009'   'ANLN2'         'TG_ITENS' 'ANLN2'         ' '             '5'  ' ' ' ' ' ',
        8 'ZFIWRT0009'   'MENGE'         'TG_ITENS' 'MENGE'         'Quantidade'    '16'  ' ' 'X' ' ',
        9 'ZFIWRT0009'   'NETPR'         'TG_ITENS' 'NETPR'         'Valor Unit.'   '16'  ' ' 'X' ' ',
       10 'ZFIWRT0009'   'NETWR'         'TG_ITENS' 'NETWR'         'Valot Total'   '16'  ' ' 'X' ' ',
       10 'ZFIWRT0009'   'BELNR_IMB'     'TG_ITENS' 'BELNR_IMB'     'Doc Vd. Imob'  '16'  ' ' ' ' 'X',
       10 'ZFIWRT0009'   'VBELN_R'       'TG_ITENS' 'VBELN_R'       'Remessa'       '16'  ' ' ' ' 'X',
       10 'ZFIWRT0009'   'TRANS_STATUS'  'TG_ITENS' 'TRANS_STATUS'  'Status Transf' '10'  ' ' ' ' 'X'.
ENDFORM.                    " MONTAR_LAYOUT_GRID2
*&---------------------------------------------------------------------*
*&      Form  f_preencher_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0380   text
*      -->P_0381   text
*      -->P_0382   text
*----------------------------------------------------------------------*
FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wl_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wl_bdc-program,
  l_value TO wl_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wl_bdc-fnam,
      l_value TO wl_bdc-fval.
  ENDIF.
  APPEND wl_bdc TO tl_bdc.
  CLEAR: wl_bdc.

ENDFORM.                    " f_preencher_dynpro
*&---------------------------------------------------------------------*
*&      Form  GERA_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_fiscal USING wl_monitor TYPE ty_monitor.
  DATA: sl_header               TYPE bapi_j_1bnfdoc,
        sl_header_add           TYPE bapi_j_1bnfdoc_add,
        tl_partner              TYPE TABLE OF bapi_j_1bnfnad,
        tl_item                 TYPE TABLE OF bapi_j_1bnflin,
        tl_item_add             TYPE TABLE OF bapi_j_1bnflin_add,
        tl_item_tax             TYPE TABLE OF bapi_j_1bnfstx,
        tl_return               TYPE TABLE OF bapiret2,
*        tl_ret        TYPE TABLE OF zsdt_retlote,
*        sl_doc        TYPE type_doc,
        tl_msg                  TYPE TABLE OF bapi_j_1bnfftx,
        tl_0010                 TYPE TABLE OF zfiwrt0010 WITH HEADER LINE,
        tl_0015                 TYPE TABLE OF zfiwrt0015 WITH HEADER LINE,
        tl_0013                 TYPE TABLE OF zfiwrt0013 WITH HEADER LINE,
        wl_1baa                 TYPE j_1baa,
        sl_partner              TYPE bapi_j_1bnfnad,
        sl_item                 TYPE bapi_j_1bnflin,
        sl_item_add             TYPE bapi_j_1bnflin_add,
        sl_item_tax             TYPE bapi_j_1bnfstx,
        sl_msg                  TYPE bapi_j_1bnfftx,
        vl_itmnum               TYPE j_1bnflin-itmnum,
        vl_answer               TYPE char1,
        vl_cfop                 TYPE j_1bcfop,
        vl_docnum               TYPE j_1bnfdoc-docnum,
        vl_nfenum               TYPE j_1bnfdoc-nfenum,
        vl_qtd                  TYPE j_1bnetqty,
        vl_netwrt               TYPE j_1bnflin-netwrt,
        vl_lifnr                TYPE vbpa-lifnr,
        sl_nfcheck              TYPE bapi_j_1bnfcheck,
        wa_material_text_record TYPE makt.

*  IF s_val-menge NE p_quant.
*    MESSAGE i836 WITH text-015.
*    EXIT.
*  ENDIF.

  READ TABLE tg_0008
    WITH KEY seq_lcto = wl_monitor-seq_lcto.

  SELECT *
    FROM zfiwrt0015
    INTO TABLE tl_0015
     WHERE seq_lcto EQ wl_monitor-seq_lcto.

  SELECT *
    FROM zfiwrt0010
    INTO TABLE tl_0010
     WHERE seq_lcto EQ wl_monitor-seq_lcto.

  SELECT *
    FROM zfiwrt0013
    INTO TABLE tl_0013
     WHERE seq_lcto EQ wl_monitor-seq_lcto.

  SELECT SINGLE *
    FROM j_1baa
    INTO wl_1baa
     WHERE nftype EQ tg_0008-nftype.


*  READ TABLE t_vinc INTO sl_vinc INDEX 1.
*  READ TABLE t_doc INTO sl_doc
*    WITH KEY docnum = sl_vinc-docnum
*    BINARY SEARCH.

* Preenchimenyto Header
  sl_header-nftype  = tg_0008-nftype.
  sl_header-doctyp  = wl_1baa-doctyp.
  sl_header-direct  = wl_1baa-direct.
  sl_header-docdat  = p_budat.
  sl_header-pstdat  = p_budat.
  sl_header-credat  = p_budat.
  sl_header-model   = wl_1baa-model.
*  sl_header-series  = 0.
*  sl_header-nfnum   = 1.
*  CONDENSE sl_header-series.
  sl_header-manual  = 'X'.
  sl_header-waerk   = 'BRL'.
  sl_header-bukrs   = tg_0008-bukrs.
  sl_header-branch  = wl_monitor-branch.
  sl_header-parvw   = tg_0008-parvw.
  sl_header-parid   = tg_0008-parid.
  sl_header-nfe     = wl_1baa-nfe.


* Preenche Header ADD
  sl_header_add-nftot = wl_monitor-netwr.

* Preenche NFCHECK
  sl_nfcheck-chekcon = 'X'.
  LOOP AT tl_0015.
* Preenche Partner
    sl_partner-parvw  = tl_0015-parvw.
    sl_partner-parid  = tl_0015-parid.

    IF tl_0015-parvw EQ 'AG'.
      sl_partner-partyp = 'C'.

    ELSEIF tl_0015-parvw EQ 'BR'.
      sl_partner-partyp = 'B'.

    ELSEIF tl_0015-parvw EQ 'LF'.
      sl_partner-partyp = 'V'.

    ENDIF.
    APPEND sl_partner TO tl_partner.

  ENDLOOP.

  LOOP AT tg_0009
    WHERE seq_lcto EQ wl_monitor-seq_lcto.

* Preenche Item
    ADD 10 TO vl_itmnum.
    CALL FUNCTION 'J_1B_MATERIAL_READ'
      EXPORTING
        matnr                = tg_0009-matnr
        val_area             = tg_0009-bwkey
        val_type             = space
        language             = sy-langu
        i_werks              = wl_monitor-branch
      IMPORTING
        nbm                  = sl_item-nbm
        matuse               = sl_item-matuse
        matorg               = sl_item-matorg
        material_text_record = wa_material_text_record
        e_matkl              = sl_item-matkl
      EXCEPTIONS
        material_not_found   = 1
        valuation_not_found  = 2
        OTHERS               = 3.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    sl_item-maktx   = wa_material_text_record-maktx.

    sl_item-itmnum  = vl_itmnum.
* ---> S4 Migration - 04/07/2023 - FTM - Início
    sl_item-matnr   = tg_0009-matnr.
    DATA(v_len) = strlen( tg_0009-matnr ).
    IF v_len > 18.
      sl_item-matnr_long = tg_0009-matnr.
    ELSE.
      sl_item-matnr      = tg_0009-matnr.
    ENDIF.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
    sl_item-bwkey   = tg_0009-bwkey.
*  sl_item-taxsit  = sl_lin-taxsit.
*    sl_item-taxsit  = '50'.
*    sl_item-taxsi2  = sl_lin-taxsi2.
    sl_item-menge   = tg_0009-menge.
    sl_item-meins   = tg_0009-meins.
    sl_item-charg   = tg_0009-charg.
    sl_item-itmtyp  = tg_0009-itmtyp.
    sl_item-werks   = tg_0009-bwkey.
    sl_item-cfop_10 = tg_0009-cfop.
    sl_item-netpr   = tg_0009-netpr."s_val-netwrt / s_val-menge.
    sl_item-netwr   = tg_0009-netwr.
    sl_item-taxlw1  = tg_0008-taxlw1.                       "'IC5'.
    sl_item-taxlw2  = tg_0008-taxlw2.                       "'I03'.
    sl_item-taxlw4  = tg_0008-taxlw4.                       "'C08'.
    sl_item-taxlw5  = tg_0008-taxlw5.                       "'P08'.
    APPEND sl_item TO tl_item.
    CLEAR: sl_item.
* Preenche Item ADD
    sl_item_add-itmnum = vl_itmnum.
    sl_item_add-direct = sl_header-direct.
    APPEND sl_item_add TO tl_item_add.
    CLEAR: sl_item_add.

** Preenche Item TAX
    LOOP AT tl_0010
      WHERE seq_lcto EQ wl_monitor-seq_lcto
        AND itmnum   EQ tg_0009-itmnum.

      sl_item_tax-itmnum = vl_itmnum.
      sl_item_tax-taxtyp = tl_0010-taxtyp.
      sl_item_tax-base   = tl_0010-base.
      sl_item_tax-rate   = tl_0010-rate.
      sl_item_tax-taxval = tl_0010-taxval.
      sl_item_tax-excbas = tl_0010-excbas.
      sl_item_tax-othbas = tl_0010-othbas.

      APPEND sl_item_tax TO tl_item_tax.
      CLEAR: sl_item_tax.
    ENDLOOP.
  ENDLOOP.

  LOOP AT tl_0013
    WHERE seq_lcto EQ wl_monitor-seq_lcto.
    sl_msg-seqnum  = tl_0013-seqnum.
    sl_msg-linnum  = tl_0013-linnum.
    sl_msg-message = tl_0013-message.
    sl_msg-manual = 'M'.
    APPEND sl_msg TO tl_msg.
    CLEAR: sl_msg.
  ENDLOOP.

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

  IF vl_docnum IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ELSE.
    PERFORM chama_log_bapi TABLES tl_return.
  ENDIF.

ENDFORM.                    " GERA_FISCAL
*&---------------------------------------------------------------------*
*&      Form  GERA_CONTABIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TG_MONITOR  text
*----------------------------------------------------------------------*
FORM gera_contabil  USING    wl_monitor TYPE ty_monitor.
  DATA: tl_zib TYPE TABLE OF zib_contabil  WITH HEADER LINE.

* 126778 - PANF - 06.11 - INICIO
  DATA: lr_katyp TYPE RANGE OF cskb-katyp.

  lr_katyp = VALUE #( ( sign = 'I'
                        option = 'EQ'
                        low = '11')
                       ( sign = 'I'
                        option = 'EQ'
                        low = '12')  ).

  IF tg_0011 IS NOT INITIAL.

    SELECT SINGLE *
      FROM tka02
      INTO @DATA(ls_tka02)
      WHERE bukrs = @wl_monitor-bukrs.

    IF sy-subrc = 0.

      SELECT kokrs, kstar, katyp
        FROM cskb
        INTO TABLE @DATA(lt_cskb)
        FOR ALL ENTRIES IN @tg_0011
        WHERE kokrs  = @ls_tka02-kokrs
              AND kstar   =  @tg_0011-hkont
              AND datbi >=  @sy-datum .

    ENDIF.

  ENDIF.

* 126778 - PANF - 06.11 - FIM

  CLEAR: tl_zib.
  REFRESH: tl_zib.
  LOOP AT tg_0011
     WHERE seq_lcto EQ wl_monitor-seq_lcto.

    READ TABLE tg_0008 INTO tg_0008
      WITH KEY seq_lcto = tg_0011-seq_lcto.

    IF sy-subrc IS INITIAL.

      CONCATENATE 'ZGF' wl_monitor-seq_lcto p_budat(4) INTO tl_zib-obj_key.
      CONCATENATE 'GF' wl_monitor-seq_lcto INTO tl_zib-xblnr.

      ADD 1 TO tl_zib-seqitem.
      MOVE: tg_0011-bschl        TO tl_zib-bschl,
            wl_monitor-branch    TO tl_zib-gsber,
            wl_monitor-bukrs     TO tl_zib-bukrs,
            '3'                  TO tl_zib-interface,
            'Gestão Emissão NF'  TO tl_zib-bktxt,
            p_bldat              TO tl_zib-bldat,
            p_budat              TO tl_zib-budat,
            p_bldat(4)           TO tl_zib-gjahr,
            p_budat(4)           TO tl_zib-monat,
            'WR'                 TO tl_zib-blart,
            tg_0011-hkont        TO tl_zib-hkont,
            tg_0011-dmbtr        TO tl_zib-wrbtr,
            'BRL'                TO tl_zib-waers,
            wl_monitor-branch    TO tl_zib-bupla,
            tg_0011-dmbtr        TO tl_zib-dmbtr,
            'N'                  TO tl_zib-rg_atualizado,
            tg_0011-newbw         TO tl_zib-newbw.

* 126778 - PANF - 06.11 - INICIO
      IF lt_cskb[ kokrs  = ls_tka02-kokrs
                  kstar   = tg_0011-hkont ]-katyp IN lr_katyp.

        tl_zib-prctr =  '9900'.

      ENDIF.
* 126778 - PANF - 06.11 - Fim


      IF tg_0011-newbw IS INITIAL.
        APPEND tl_zib.

      ELSE.
        LOOP AT tg_0009
          WHERE seq_lcto EQ wl_monitor-seq_lcto.
          MOVE: tg_0009-anln1 TO tl_zib-anln1,
                tg_0009-anln2 TO tl_zib-anln2.

          APPEND tl_zib.
        ENDLOOP.
      ENDIF.
      CLEAR: tl_zib, tg_0008.
    ENDIF.
  ENDLOOP.

  MODIFY zib_contabil FROM TABLE tl_zib.

ENDFORM.                    " GERA_CONTABIL
*&---------------------------------------------------------------------*
*&      Form  GERA_DOC_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TG_MONITOR  text
*----------------------------------------------------------------------*
FORM gera_doc_material  USING   wl_monitor TYPE ty_monitor.
  DATA: wl_header TYPE bapi2017_gm_head_01,
        wl_code   TYPE bapi2017_gm_code,
        wl_mblnr  TYPE zfiwrt0008-mblnr,
        wl_mjahr  TYPE mjahr,
        tl_item   TYPE TABLE OF bapi2017_gm_item_create WITH HEADER LINE,
        tl_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

  CLEAR: wl_header, tl_item, wl_code.
  REFRESH: tl_item.

  wl_header-pstng_date = p_budat.
  wl_header-doc_date   = p_budat.
  wl_code-gm_code      = '06'.

  LOOP AT tg_0009
    WHERE seq_lcto EQ wl_monitor-seq_lcto.

    READ TABLE tg_0012
      WITH KEY seq_lcto = wl_monitor-seq_lcto.



*---> 19/06/2023 - Migração S4 - DG
    "MOVE: TG_0009-MATNR     TO TL_ITEM-MATERIAL,

    DATA(v_len) = strlen( tg_0009-matnr ).

    IF v_len > 18.
      MOVE: tg_0009-matnr     TO tl_item-material_long.
    ELSE.
      MOVE: tg_0009-matnr     TO tl_item-material.
    ENDIF.

    MOVE:
*<--- 19/06/2023 - Migração S4 - DG

          wl_monitor-branch TO tl_item-plant,
          tg_0012-bwart     TO tl_item-move_type,
          tg_0009-lgort     TO tl_item-stge_loc,
          tg_0009-charg     TO tl_item-batch,
          tg_0009-menge     TO tl_item-entry_qnt.

    APPEND tl_item.
    CLEAR: tl_item, tg_0012.
  ENDLOOP.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      goodsmvt_header  = wl_header
      goodsmvt_code    = wl_code
    IMPORTING
      materialdocument = wl_mblnr
      matdocumentyear  = wl_mjahr
    TABLES
      goodsmvt_item    = tl_item
      return           = tl_return.

  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ELSE.
    PERFORM chama_log_bapi TABLES tl_return.
  ENDIF.


ENDFORM.                    " GERA_DOC_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  CHAMA_LOG_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_RETURN  text
*----------------------------------------------------------------------*
FORM chama_log_bapi  TABLES   tl_return STRUCTURE zfiwrt1000.
  DATA: BEGIN OF tl_log_bapi OCCURS 0,
          tipo      TYPE c,
          line(255) TYPE c,
        END OF tl_log_bapi.

  DATA: wl_layout TYPE slis_layout_alv.

  LOOP AT tl_return.
    MOVE: tl_return-type     TO tl_log_bapi-tipo,
          tl_return-mensagem TO tl_log_bapi-line.

    APPEND tl_log_bapi.

  ENDLOOP.

  REFRESH:estrutura.

  PERFORM montar_layout_log.
*  V_REPORT = SY-REPID.
  wl_layout-zebra = c_x.
  wl_layout-colwidth_optimize = c_x.

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
ENDFORM.                    " CHAMA_LOG_BAPI
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_log.
  PERFORM montar_estrutura_log USING:
        1 ' '  ' ' 'TL_LOG_BAPI' 'TIPO'  'Tipo de Msg.'  ' ',
        2 ' '  ' ' 'TL_LOG_BAPI' 'LINE'  'Log. de Exec.'  ' ' .


ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura_log USING VALUE(p_col_pos)       TYPE i
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
*&---------------------------------------------------------------------*
*&      Form  ALTERA_STATUS_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TG_MONITOR  text
*----------------------------------------------------------------------*
FORM altera_status_proc  USING  wl_monitor TYPE ty_monitor.
  DATA: wl_cont(1)     TYPE i,
        wl_cont_aux(1).
*  READ TABLE tg_0008
*    WITH KEY seq_lcto = wl_monitor-seq_lcto.

  SELECT SINGLE *
    FROM zfiwrt0008
    INTO tg_0008
     WHERE seq_lcto EQ wl_monitor-seq_lcto.

  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'ENQUEUE_EZFIWRT0008'
      EXPORTING
*       MODE_ZFIWRT0008       = 'E'
*       MANDT          = SY-MANDT
        seq_lcto       = wl_monitor-seq_lcto
*       X_SEQ_LCTO     = ' '
*       _SCOPE         = '2'
*       _WAIT          = ' '
*       _COLLECT       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      IF tg_0008-loekz IS INITIAL.
        IF tg_0008-obj_key IS INITIAL.
          CONCATENATE 'ZG0' wl_monitor-seq_lcto p_budat(4) INTO tg_0008-obj_key.

        ELSE.
          READ TABLE tg_zib_cont
            WITH KEY obj_key = tg_0008-obj_key
                       BINARY SEARCH.
          IF tg_zib_cont-rg_atualizado EQ 'S'.

            READ TABLE tg_zib_err
              WITH KEY obj_key = tg_0008-obj_key
                          type = 'E'
                         BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              wl_cont =  tg_0008-obj_key+2(1).
              ADD 1 TO wl_cont.
              wl_cont_aux = wl_cont.
              CONCATENATE 'ZG' wl_cont_aux wl_monitor-seq_lcto p_budat(4) INTO tg_0008-obj_key.

            ENDIF.
          ENDIF.
        ENDIF.
        MOVE: c_a     TO tg_0008-status.
*          P_BUDAT TO TG_0008-BUDAT,
*          P_BLDAT TO TG_0008-BLDAT.
        CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
          EXPORTING
            seq_lcto = wl_monitor-seq_lcto.

        MODIFY zfiwrt0008 FROM tg_0008.


*    COMMIT WORK AND WAIT.
      ELSE.

      ENDIF.
      CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
        EXPORTING
          seq_lcto = wl_monitor-seq_lcto.
    ENDIF.
  ENDIF.

ENDFORM.                    " ALTERA_STATUS_PROC
*&---------------------------------------------------------------------*
*&      Module  SEARCH_OPER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_oper INPUT.
  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.
  DATA: BEGIN OF t_fieldtab OCCURS 3.
          INCLUDE STRUCTURE dynpread.
  DATA: END OF t_fieldtab.

  DATA: BEGIN OF tl_operacao OCCURS 0,
          operacao  TYPE zfiwrt0001-operacao,
          descricao TYPE zfiwrt0001-descricao,
        END OF tl_operacao.

  REFRESH: tl_operacao, t_fieldtab.
  CLEAR:   tl_operacao, t_fieldtab.

  SELECT operacao descricao
    FROM zfiwrt0001
    INTO TABLE tl_operacao
     WHERE opr_blq EQ 'L'.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OPERACAO'
      dynpprog        = sy-repid                            "'ZFINR018'
      dynpnr          = sy-dynnr
      dynprofield     = 'P_OPERA'
      value_org       = 'S'
    TABLES
      value_tab       = tl_operacao
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " SEARCH_OPER  INPUT



FORM f_disp_cct.

  DATA: ls_zlest0142 TYPE zlest0142.

  DATA: v_candat    TYPE j_1bnfdoc-candat,
        v_qtde_nota TYPE j_1bnflin-menge,
        v_chave     TYPE c LENGTH 44,
        v_menge_aux TYPE j_1bnflin-menge,
        var_answer  TYPE c,
        v_filial    TYPE c LENGTH 10.

  DATA: it_sel_rows TYPE lvc_t_row,
        wa_sel_rows TYPE lvc_s_row.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD grid1->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  IF it_sel_rows[] IS INITIAL.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente enviar o(s) registro(s) selecionado(s) para o CCT?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    CLEAR: ls_zlest0142.

    READ TABLE tg_monitor ASSIGNING FIELD-SYMBOL(<saida_0100>) INDEX wa_sel_rows-index.

    CHECK ( sy-subrc EQ 0 ) AND
          ( <saida_0100>-seq_lcto IS NOT INITIAL     ) AND
          ( <saida_0100>-disp_cct EQ icon_led_yellow ).

    SELECT SINGLE *
      FROM zfiwrt0008 INTO @DATA(_wl_0008)
     WHERE seq_lcto EQ @<saida_0100>-seq_lcto.

    IF sy-subrc NE 0.
      MESSAGE | Registro ZNFW com Seq.Lcto: { <saida_0100>-seq_lcto } não encontrado! | TYPE 'S'.
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      FROM zfiwrt0015 INTO @DATA(_wl_0015)
     WHERE seq_lcto EQ @<saida_0100>-seq_lcto
       AND parvw    EQ 'Z1'. "Terminal porto

    IF ( sy-subrc NE 0 ) OR ( _wl_0015-parid IS INITIAL ).
      MESSAGE | Parceiro Z1 Seq.Lcto: { _wl_0008-seq_lcto } não econtrado! | TYPE 'S'.
      CONTINUE.
    ENDIF.

    v_filial = _wl_0015-parid.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = v_filial
      IMPORTING
        output = v_filial.

    IF strlen( v_filial ) > 4.
      MESSAGE | Parceiro Z1 Seq.Lcto: { _wl_0008-seq_lcto } não é uma Filial! | TYPE 'S'.
      CONTINUE.
    ENDIF.

    v_filial = _wl_0015-parid+6(4).

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(_wl_branch)
     WHERE branch = @v_filial.

    IF ( sy-subrc NE 0 ).
      MESSAGE | Cadastro Filial: { v_filial } não encontrado. Seq.Lcto: { _wl_0008-seq_lcto }! | TYPE 'S'.
      CONTINUE.
    ENDIF.

    IF ( _wl_0008-st_cct EQ '01' ) OR ( _wl_0008-st_cct EQ '02' ).
      MESSAGE | Docnum { _wl_0008-docnum } já disponibilizado para registro CCT!'| TYPE 'S'.
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      FROM zfiwrt0001 INTO @DATA(_wl_0001)
     WHERE operacao EQ @_wl_0008-operacao.

    IF ( sy-subrc NE 0 ) OR ( _wl_0001-disp_nf_cct NE 'S' ) OR ( _wl_0008-operacao IS INITIAL ).
      MESSAGE | Operação: { _wl_0008-operacao } não configurada para disponibilizar NF para CCT! | TYPE 'S'.
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      FROM zfiwrt0019 INTO @DATA(_wl_0019)
     WHERE seq_lcto EQ @_wl_0008-seq_lcto.

    IF ( sy-subrc NE 0 ) OR ( _wl_0019-lifnr IS INITIAL ).
      IF ( _wl_0008-operacao EQ '0019' ) AND ( _wl_0008-dt_criacao <= '20180930' ). "Dt Corte.
        _wl_0019-lifnr = _wl_0015-parid. "Terminal Porto
      ELSE.
        MESSAGE | Dados Transporte Seq.Lcto: { _wl_0008-seq_lcto } não econtrado ou está incompleto! | TYPE 'S'.
        CONTINUE.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = _wl_0019-lifnr
      IMPORTING
        output = _wl_0019-lifnr.

    SELECT SINGLE *
      FROM lfa1 INTO @DATA(_wl_lfa1_transp)
     WHERE lifnr = @_wl_0019-lifnr.

    IF ( sy-subrc NE 0 ) OR ( _wl_lfa1_transp-stcd1 IS INITIAL AND _wl_lfa1_transp-stcd2 IS INITIAL ).
      MESSAGE | Fornecedor: { _wl_0019-lifnr } não econtrado ou não possui CNPJ/CPF informado! | TYPE 'S'.
      CONTINUE.
    ENDIF.

    IF _wl_0008-docnum IS INITIAL.
      MESSAGE | Documento Fiscal ainda não gerado! | TYPE 'S'.
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      FROM j_1bnfe_active INTO @DATA(_wl_active)
     WHERE docnum     = @_wl_0008-docnum
       AND docsta     = '1'
       AND cancel     = ''.

    IF sy-subrc NE 0.
      MESSAGE | Documento Fiscal não está ativo/autorizado! | TYPE 'S'.
      CONTINUE.
    ENDIF.

    CLEAR: v_candat.
    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(_wl_j_1bnfdoc)
     WHERE docnum     = @_wl_0008-docnum
       AND candat     = @v_candat.

    IF sy-subrc NE 0.
      MESSAGE | Documento Fiscal não está ativo/autorizado! | TYPE 'S'.
      CONTINUE.
    ENDIF.

    SELECT *
      FROM j_1bnflin INTO TABLE @DATA(it_j_1bnflin)
     WHERE docnum     = @_wl_0008-docnum.

    IF sy-subrc NE 0.
      MESSAGE | Item Documento Fiscal não encontrado! | TYPE 'S'.
      CONTINUE.
    ENDIF.

    CALL METHOD zcl_util=>monta_chave_nfe
      EXPORTING
        i_docnum = _wl_0008-docnum
        i_valida = 'X'
      RECEIVING
        e_chave  = v_chave.

    IF ( v_chave IS INITIAL ) OR ( strlen( v_chave ) NE 44 ).
      MESSAGE |Docnum: { _wl_0008-docnum } com chave NF-e incompleta!| TYPE 'S'.
      RETURN.
    ENDIF.

    ls_zlest0142-seq_lcto         = _wl_0008-seq_lcto.
    ls_zlest0142-chave_nfe        = v_chave.
    ls_zlest0142-cnpj_emissor     = v_chave+06(14).
    ls_zlest0142-numero           = v_chave+25(9).
    ls_zlest0142-serie            = v_chave+22(3).
    ls_zlest0142-dt_emissao       = _wl_j_1bnfdoc-docdat.
    ls_zlest0142-model            = v_chave+20(2).
    ls_zlest0142-dt_chegada       = _wl_j_1bnfdoc-docdat.
    ls_zlest0142-bukrs_rom        = _wl_branch-bukrs.
    ls_zlest0142-branch_rom       = _wl_branch-branch.
    ls_zlest0142-bukrs_ra         = _wl_branch-bukrs.
    ls_zlest0142-branch_ra        = _wl_branch-branch.
    ls_zlest0142-docnum           = _wl_0008-docnum.
    ls_zlest0142-sem_romaneio     = abap_true.

    "LS_ZLEST0142-MODAL            =

    IF _wl_lfa1_transp-stcd1 IS NOT INITIAL.
      ls_zlest0142-cnpj_transp    = _wl_lfa1_transp-stcd1.
    ELSE.
      ls_zlest0142-cpf_transp     = _wl_lfa1_transp-stcd2.
    ENDIF.

    ls_zlest0142-data_reg         = sy-datum.
    ls_zlest0142-hora_reg         = sy-uzeit.

    CLEAR: v_qtde_nota.
    LOOP AT it_j_1bnflin INTO DATA(_wl_lin).
      IF _wl_lin-meins NE 'KG'.

        CLEAR: v_menge_aux.

        CALL FUNCTION 'ME_CONVERSION_MEINS'
          EXPORTING
            i_matnr             = _wl_lin-matnr
            i_mein1             = _wl_lin-meins
            i_meins             = 'KG'
            i_menge             = _wl_lin-menge
          IMPORTING
            menge               = v_menge_aux
          EXCEPTIONS
            error_in_conversion = 1
            no_success          = 2
            OTHERS              = 3.

        IF sy-subrc NE 0.
          MESSAGE |Erro conversão quantidade documento { _wl_lin-docnum }! | TYPE 'I'.
          CONTINUE.
        ENDIF.

        ADD v_menge_aux TO v_qtde_nota.
      ELSE.
        ADD _wl_lin-menge TO v_qtde_nota.
      ENDIF.

      ls_zlest0142-matnr        = _wl_lin-matnr.
    ENDLOOP.

    IF v_qtde_nota IS INITIAL.
      MESSAGE |Quantidade do documento: { _wl_lin-docnum } não encontrada! | TYPE 'I'.
      CONTINUE.
    ENDIF.

    ls_zlest0142-peso_chegada     = v_qtde_nota.
    ls_zlest0142-peso_fiscal      = v_qtde_nota.

    FREE zcl_cct_control_nf.
    CREATE OBJECT zcl_cct_control_nf.
    zcl_cct_control_nf->atribuir_nf( i_zlest0142 = ls_zlest0142 ).
    DATA(_disponibilizada) = zcl_cct_control_nf->disp_nf_cct( ).

    IF _disponibilizada EQ abap_true.
      <saida_0100>-disp_cct = icon_okay.
      DATA(_disp_cct) = 'X'.
    ENDIF.

  ENDLOOP.

  IF _disp_cct IS NOT INITIAL.
    MESSAGE 'Notas selecionada(s), disponibilizada(s) para registro de CCT com sucesso!' TYPE 'S'.
    LEAVE TO SCREEN 0100.
  ENDIF.

ENDFORM.


FORM f_remover_cct.

  DATA: v_chave    TYPE c LENGTH 44,
        var_answer TYPE c.

  DATA: it_sel_rows TYPE lvc_t_row,
        wa_sel_rows TYPE lvc_s_row.

  DATA: lt_zlest0142 TYPE TABLE OF zlest0142,
        ls_zlest0142 TYPE zlest0142.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD grid1->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente remover o(s) registro(s) selecionado(s) do CCT?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  DATA(_rem_cct) = abap_false.
  LOOP AT it_sel_rows INTO wa_sel_rows.

    CLEAR: ls_zlest0142.

    READ TABLE tg_monitor ASSIGNING FIELD-SYMBOL(<saida_0100>) INDEX wa_sel_rows-index.

    CHECK ( sy-subrc = 0                         ) AND
          ( <saida_0100>-seq_lcto IS NOT INITIAL ) AND
          ( <saida_0100>-disp_cct = icon_okay    ).

    SELECT SINGLE *
      FROM zfiwrt0008 INTO @DATA(_wl_0008)
     WHERE seq_lcto EQ @<saida_0100>-seq_lcto.

    IF sy-subrc NE 0.
      MESSAGE | Registro ZNFW com Seq.Lcto: { <saida_0100>-seq_lcto } não encontrado! | TYPE 'S'.
      CONTINUE.
    ENDIF.

    IF _wl_0008-docnum IS INITIAL.
      MESSAGE | Documento Fiscal ainda não gerado! | TYPE 'S'.
      CONTINUE.
    ENDIF.

    CALL METHOD zcl_util=>monta_chave_nfe
      EXPORTING
        i_docnum = _wl_0008-docnum
        i_valida = 'X'
      RECEIVING
        e_chave  = v_chave.

    IF ( v_chave IS INITIAL ) OR ( strlen( v_chave ) NE 44 ).
      MESSAGE |Docnum: { _wl_0008-docnum } com chave NF-e incompleta!| TYPE 'S'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zlest0142 INTO ls_zlest0142
     WHERE seq_lcto EQ _wl_0008-seq_lcto.

    IF ( sy-subrc = 0 ).

      FREE zcl_cct_control_nf.
      CREATE OBJECT zcl_cct_control_nf.
      DATA(_removida) = zcl_cct_control_nf->remover_nf_cct( i_zlest0142 = ls_zlest0142 ).
      IF _removida EQ abap_true.
        _rem_cct = abap_true.
        <saida_0100>-disp_cct = icon_led_yellow.
      ENDIF.

    ELSE.

      UPDATE zfiwrt0008 SET st_cct = space " Não utilizado
       WHERE seq_lcto EQ <saida_0100>-seq_lcto.

      _rem_cct = abap_true.
      <saida_0100>-disp_cct = icon_led_yellow.

    ENDIF.

  ENDLOOP.

  IF _rem_cct IS NOT INITIAL.
    MESSAGE 'Notas selecionada(s), removida(s) do registro de CCT com sucesso!' TYPE 'S'.
    LEAVE TO SCREEN 0100.
  ENDIF.

ENDFORM.

FORM f_selecao_externa TABLES p_zfiwrt0008_ext STRUCTURE zfiwrt0008
                    CHANGING c_saida_znfw0005_t TYPE zfiwrs0004_t.

  DATA: wl_saida_znfw0005 TYPE zfiwrs0004.

  REFRESH: c_saida_znfw0005_t, tg_monitor.

  LOOP AT p_zfiwrt0008_ext INTO DATA(wl_zfiwrt0008).
    CLEAR: rg_seq_lcto_ext.
    rg_seq_lcto_ext-sign   = 'I'.
    rg_seq_lcto_ext-option = 'EQ'.
    rg_seq_lcto_ext-low    = wl_zfiwrt0008-seq_lcto.
    APPEND rg_seq_lcto_ext.
  ENDLOOP.

  CHECK rg_seq_lcto_ext[] IS NOT INITIAL.

  PERFORM busca_dados.

  LOOP AT tg_monitor.
    CLEAR: wl_saida_znfw0005.

    wl_saida_znfw0005-seq_lcto          = tg_monitor-seq_lcto.
    wl_saida_znfw0005-status            = tg_monitor-status.
    wl_saida_znfw0005-status_proc       = tg_monitor-status_proc.
    wl_saida_znfw0005-docnum_flag       = tg_monitor-docnum_flag.
    wl_saida_znfw0005-docnum            = tg_monitor-docnum.
    wl_saida_znfw0005-doc_contab_flag   = tg_monitor-doc_contab_flag.
    wl_saida_znfw0005-doc_contab        = tg_monitor-doc_contab.
    wl_saida_znfw0005-belnr             = tg_monitor-belnr.
    wl_saida_znfw0005-belnr_flag        = tg_monitor-belnr_flag.
    wl_saida_znfw0005-vbeln_r           = tg_monitor-vbeln_r.
    wl_saida_znfw0005-vbeln_flag        = tg_monitor-vbeln_flag.
    wl_saida_znfw0005-doc_mat_flag      = tg_monitor-doc_mat_flag.
    wl_saida_znfw0005-doc_mat           = tg_monitor-doc_mat.
    wl_saida_znfw0005-doc_transf_flag   = tg_monitor-doc_transf_flag.
    wl_saida_znfw0005-doc_transf        = tg_monitor-doc_transf.
    wl_saida_znfw0005-nivel_aprov       = tg_monitor-nivel_aprov.
    wl_saida_znfw0005-aprov             = tg_monitor-aprov.

    APPEND wl_saida_znfw0005 TO c_saida_znfw0005_t.

  ENDLOOP.
ENDFORM.
