
*&---------------------------------------------------------------------*
*& Report  ZFI_IMP_IB_CONTABIL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfi_imp_ib_contabil.

*  INCLUDE ole2incl.
*  INCLUDE docsincl.
*
**----------------------------------------------------------------------*
**                                 TYPES                                *
**----------------------------------------------------------------------*
*TYPES: BEGIN OF TYPE_LINE,
*         LINE TYPE CHAR600,
*       END   OF TYPE_LINE.
*
**----------------------------------------------------------------------*
**                                Work Areas                            *
**----------------------------------------------------------------------*
*DATA: WA_IB_CONTABIL TYPE ZIB_CONTABIL.
*
*
**----------------------------------------------------------------------*
**                                TABELAS                               *
**----------------------------------------------------------------------*
*DATA: T_LINE         TYPE TABLE OF TYPE_LINE,
*      it_IB_CONTABIL TYPE TABLE OF ZIB_CONTABIL.
*
**----------------------------------------------------------------------*
**                               CONSTANTES                             *
**----------------------------------------------------------------------*
*CONSTANTS: C_FILETXT   TYPE CHAR26 VALUE 'Files CSV (*.XLSX)|*.XLSX|'  ,
*           C_INICIAL   TYPE CHAR3  VALUE 'C:\'                       ,
*           C_X         TYPE CHAR1  VALUE 'X'                         .
*
*
**----------------------------------------------------------------------*
**                            TELA DE SELEÇÂO                           *
**----------------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-001.
*  SELECTION-SCREEN BEGIN OF BLOCK A2 WITH FRAME.
*    PARAMETERS P_FILE   TYPE SDBA_A_NAM OBLIGATORY.
*  SELECTION-SCREEN END   OF BLOCK A2.
*SELECTION-SCREEN END   OF BLOCK A1.
*
**----------------------------------------------------------------------*
**                         AT SELECTION SCREEN                          *
**----------------------------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
*
** Ajuda Pesquiza Campo Filename
*  PERFORM Z_AJUDA_FILENAME.
*
**----------------------------------------------------------------------*
**                         START OF SELECTION                           *
**----------------------------------------------------------------------*
*START-OF-SELECTION.
*
** Upload do Arquivo
*  PERFORM: ZIMPORTA_EXCEL  .
*
**&---------------------------------------------------------------------*
**&      Form  Z_AJUDA_FILENAME                                         *
**&---------------------------------------------------------------------*
**                      Ajuda Pesquiza Campo Filename                   *
**----------------------------------------------------------------------*
*FORM Z_AJUDA_FILENAME.
*
*  DATA: VL_TITLE   TYPE STRING    ,
*        VL_FILTER  TYPE STRING    ,
*        VL_INITIAL TYPE STRING    ,
*        VL_RC      TYPE I         ,
*        TL_FILE    TYPE FILETABLE .
*
*  VL_TITLE   = TEXT-002.
*  VL_FILTER  = C_FILETXT.
*  VL_INITIAL = C_INICIAL.
*
*  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
*    EXPORTING
*      WINDOW_TITLE            = VL_TITLE
*      FILE_FILTER             = VL_FILTER
*      INITIAL_DIRECTORY       = VL_INITIAL
*    CHANGING
*      FILE_TABLE              = TL_FILE
*      RC                      = VL_RC
*    EXCEPTIONS
*      FILE_OPEN_DIALOG_FAILED = 1
*      CNTL_ERROR              = 2
*      ERROR_NO_GUI            = 3
*      NOT_SUPPORTED_BY_GUI    = 4
*      OTHERS                  = 5.
*
*  IF NOT SY-SUBRC IS INITIAL.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    EXIT.
*  ENDIF.
*
*  READ TABLE TL_FILE INTO P_FILE INDEX 1.
*
*ENDFORM.                    " Z_AJUDA_FILENAME
*
*
*
*FORM ZIMPORTA_EXCEL.
*
*  DATA: h_appl          LIKE obj_record,
*        h_work          LIKE obj_record,
*        h_cell          LIKE obj_record,
*        excel_line      LIKE sy-index,
*        excel_coln      LIKE sy-index,
*        cell_value(132) TYPE c,
*        VL_FILE         TYPE STRING,
*        p_line          TYPE INT4,
*        vfim_planilha   type c length 1 .
*
*  IF h_appl-header = space OR h_appl-handle = -1.
*    CREATE OBJECT h_appl 'EXCEL.APPLICATION'.
*    IF sy-subrc NE 0. MESSAGE i002(sy) WITH sy-msgli. ENDIF.
*    SET PROPERTY OF h_appl 'VISIBLE' = 0.
*  ENDIF.
*
*  CALL METHOD OF h_appl 'WORKBOOKS' = h_work.
*  CALL METHOD OF h_work 'OPEN' EXPORTING #1 = P_FILE.
*
*  p_line = 1.
*
*  vfim_planilha = 'N'.
*
*  while vfim_planilha = 'N'.
*
*    p_line = p_line + 1.
*
*    CLEAR: WA_IB_CONTABIL.
*
*    PERFORM LER_CELULA USING h_cell h_appl P_LINE 1 'N' CHANGING WA_IB_CONTABIL-OBJ_KEY.
*    if WA_IB_CONTABIL-OBJ_KEY is not initial.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 2  'N' CHANGING WA_IB_CONTABIL-SEQITEM.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 3  'N' CHANGING WA_IB_CONTABIL-BSCHL.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 4  'N' CHANGING WA_IB_CONTABIL-GSBER.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 5  'N' CHANGING WA_IB_CONTABIL-BUKRS.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 6  'N' CHANGING WA_IB_CONTABIL-INTERFACE.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 7  'N' CHANGING WA_IB_CONTABIL-BKTXT.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 8  'N' CHANGING WA_IB_CONTABIL-BLDAT.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 9  'N' CHANGING WA_IB_CONTABIL-BUDAT.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 10 'N' CHANGING WA_IB_CONTABIL-GJAHR.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 11 'N' CHANGING WA_IB_CONTABIL-MONAT.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 12 'N' CHANGING WA_IB_CONTABIL-BLART.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 13 'N' CHANGING WA_IB_CONTABIL-XBLNR.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 14 'N' CHANGING WA_IB_CONTABIL-HKONT.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 15 'S' CHANGING WA_IB_CONTABIL-WRBTR.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 16 'N' CHANGING WA_IB_CONTABIL-WAERS.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 17 'N' CHANGING WA_IB_CONTABIL-ZFBDT.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 18 'N' CHANGING WA_IB_CONTABIL-ZLSPR.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 19 'N' CHANGING WA_IB_CONTABIL-ZLSCH.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 20 'N' CHANGING WA_IB_CONTABIL-KIDNO.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 21 'N' CHANGING WA_IB_CONTABIL-SGTXT.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 22 'N' CHANGING WA_IB_CONTABIL-XREF1.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 23 'N' CHANGING WA_IB_CONTABIL-XREF2.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 24 'N' CHANGING WA_IB_CONTABIL-XREF3.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 25 'N' CHANGING WA_IB_CONTABIL-BUPLA.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 26 'N' CHANGING WA_IB_CONTABIL-ZUONR.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 27 'N' CHANGING WA_IB_CONTABIL-UMSKZ.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 28 'N' CHANGING WA_IB_CONTABIL-KOSTL.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 29 'N' CHANGING WA_IB_CONTABIL-AUFNR.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 30 'N' CHANGING WA_IB_CONTABIL-PRCTR.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 31 'N' CHANGING WA_IB_CONTABIL-WAERS_I.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 32 'S' CHANGING WA_IB_CONTABIL-DMBTR.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 33 'N' CHANGING WA_IB_CONTABIL-WAERS_F.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 34 'S' CHANGING WA_IB_CONTABIL-DMBE2.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 35 'N' CHANGING WA_IB_CONTABIL-BVTYP.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 36 'N' CHANGING WA_IB_CONTABIL-HBKID.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 37 'N' CHANGING WA_IB_CONTABIL-RG_ATUALIZADO.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 38 'N' CHANGING WA_IB_CONTABIL-BANKL.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 39 'N' CHANGING WA_IB_CONTABIL-BANKN.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 40 'N' CHANGING WA_IB_CONTABIL-NEWBW.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 41 'N' CHANGING WA_IB_CONTABIL-ANLN1.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 42 'N' CHANGING WA_IB_CONTABIL-ANLN2.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 43 'N' CHANGING WA_IB_CONTABIL-BEWAR.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 44 'N' CHANGING WA_IB_CONTABIL-WAERS_G.
*       PERFORM LER_CELULA USING h_cell h_appl P_LINE 45 'N' CHANGING WA_IB_CONTABIL-DMBE3.
*
*       append WA_IB_CONTABIL to it_IB_CONTABIL.
*    else.
*      vfim_planilha = 'S'.
*    endif.
*
*
*
*  endwhile.
*
*  modify ZIB_CONTABIL from table it_IB_CONTABIL.
*
** Release Excel
*  CALL METHOD OF h_appl 'QUIT'.
*
*  FREE OBJECT h_appl.
*  h_appl-handle = -1.
*
*  message I000(Z01) WITH 'Arquivo importado '.
*
*
*ENDFORM.
*
**&---------------------------------------------------------------------*
**&      Form  LER_CELULA
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM LER_CELULA USING h_cell h_appl P_LINE P_COLUN decimal CHANGING  v_value.
*  data: cell_value(132) TYPE c.
*
*  CALL METHOD OF h_appl 'CELLS' = h_cell
*      EXPORTING
*        #1 = p_line
*        #2 = p_colun.
*
*  GET PROPERTY OF h_cell 'VALUE' = cell_value.
*
*  if decimal = 'N' .
*    REPLACE '.000000' with space into cell_value.
*  endif.
*
*  v_value = cell_value.
*
*ENDFORM.                    " LER_CELULA


*--------------------------------------------------------------------------------------------------*
* Developer: Enio Jesus
* Data.....: 08.10.2015
*--------------------------------------------------------------------------------------------------*

TYPE-POOLS icon.

TYPES: BEGIN OF ty_saida,
         status  TYPE icon-id,
         obj_key TYPE char20,
         belnr   TYPE char10,
         seqitem TYPE num6,
         bukrs   TYPE bukrs,
         descr   TYPE char50,
       END OF ty_saida.

DATA: obj_custom         TYPE REF TO cl_gui_custom_container,
      obj_custom_log     TYPE REF TO cl_gui_container,
      obj_splitter       TYPE REF TO cl_gui_splitter_container,
      obj_alv            TYPE REF TO cl_gui_alv_grid,
      gt_planilha        LIKE STANDARD TABLE OF alsmex_tabline,
      gt_msg_return      TYPE TABLE OF zfiwrs0002,
      gt_zibcontabil     TYPE TABLE OF zib_contabil,
      gt_zibcontabil_err TYPE TABLE OF zib_contabil_err,
      gt_fcat            TYPE TABLE OF lvc_s_fcat,
      gt_saida           TYPE TABLE OF ty_saida,
      wl_planilha        LIKE alsmex_tabline,
      wl_msg_return      TYPE zfiwrs0002,
      wl_saida           TYPE ty_saida,
      wl_zibcontabil     TYPE zib_contabil,
      wl_layout          TYPE lvc_s_layo,
      wl_mensagem        TYPE char30,
      wl_stable          TYPE lvc_s_stbl,
      wl_zibcontabil_chv TYPE zib_contabil_chv,
      wl_zibcontabil_err TYPE zib_contabil_err,
      wl_toolbar         TYPE stb_button,
      ok_code            LIKE sy-ucomm,
      p_file             TYPE rlgrap-filename.

START-OF-SELECTION.

  CALL SCREEN 0100.

*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_toolbar DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      set_toolbar  FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm   FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "LCL_EVENT_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_toolbar IMPLEMENTATION.

  METHOD set_toolbar.
    CLEAR: wl_toolbar.

    wl_toolbar-butn_type    = 3.
    APPEND wl_toolbar TO e_object->mt_toolbar.
    CLEAR wl_toolbar.

    wl_toolbar-function     = 'BTN_ATUALIZAR'.
    wl_toolbar-icon         = icon_refresh.
    wl_toolbar-butn_type    = 0.
    wl_toolbar-text         = 'Atualizar'.

    APPEND wl_toolbar TO e_object->mt_toolbar.
    CLEAR wl_toolbar.
  ENDMETHOD.                    "SET_TOOLBAR

  METHOD get_ucomm.
    CASE e_ucomm.
      WHEN 'BTN_ATUALIZAR'.
        DATA: at_index TYPE sy-tabix,
              obj_key  TYPE char20.

        CLEAR: gt_msg_return.

        LOOP AT gt_saida INTO wl_saida.
          at_index = sy-tabix.

          CLEAR gt_zibcontabil_err.

          SELECT SINGLE *
            FROM zib_contabil_chv
            INTO wl_zibcontabil_chv
           WHERE obj_key = wl_saida-obj_key.

          IF ( sy-subrc IS INITIAL ).
            wl_saida-status = icon_green_light.
            wl_saida-belnr = wl_zibcontabil_chv-belnr.

          ELSE.
            SELECT *
              FROM zib_contabil_err
              INTO TABLE gt_zibcontabil_err
             WHERE obj_key = wl_saida-obj_key.

            IF ( sy-subrc IS INITIAL ).
              wl_saida-status = icon_red_light.

              IF ( obj_key NE wl_saida-obj_key ).
                obj_key = wl_saida-obj_key.

                LOOP AT gt_zibcontabil_err INTO wl_zibcontabil_err.
                  CONCATENATE 'Objkey:' obj_key '-' wl_zibcontabil_err-message
                  INTO wl_msg_return-msg SEPARATED BY space.

                  APPEND wl_msg_return TO gt_msg_return.
                  CLEAR wl_msg_return.
                ENDLOOP.
              ENDIF.

            ENDIF.
          ENDIF.
          MODIFY gt_saida FROM wl_saida INDEX sy-tabix TRANSPORTING status
                                                                     belnr.
        ENDLOOP.

        IF ( NOT gt_msg_return IS INITIAL ).
          PERFORM show_msg.
        ENDIF.

        MESSAGE s836(sd) WITH TEXT-s01 DISPLAY LIKE 'S'.

        CALL METHOD obj_alv->refresh_table_display
          EXPORTING
            is_stable = wl_stable.

    ENDCASE.
  ENDMETHOD.                    "GET_UCOMM
ENDCLASS.                    "LCL_EVENT_TOOLBAR IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  TRATAR_EXCEL
*&---------------------------------------------------------------------*
FORM tratar_arquivo.
  REFRESH: gt_planilha, gt_saida, gt_zibcontabil.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = TEXT-i01.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 56
      i_end_row               = 10000
    TABLES
      intern                  = gt_planilha
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  LOOP AT gt_planilha INTO wl_planilha.
    AT NEW row.
      CLEAR wl_zibcontabil.
    ENDAT.

    IF wl_planilha-value(1) = space.
      SHIFT wl_planilha-value LEFT DELETING LEADING space.
    ENDIF.

    CASE wl_planilha-col.
      WHEN 1.
        IF ( wl_planilha-value NE wl_zibcontabil_err-obj_key ).
          SELECT SINGLE *
            FROM zib_contabil_err
            INTO wl_zibcontabil_err
           WHERE obj_key = wl_planilha-value.

          IF ( sy-subrc IS INITIAL ).
            DELETE FROM zib_contabil_err WHERE obj_key = wl_planilha-value.
            DELETE FROM zib_contabil     WHERE obj_key = wl_planilha-value.
            COMMIT WORK.
          ENDIF.
        ENDIF.

        wl_zibcontabil-obj_key = wl_planilha-value.
      WHEN 2.
        wl_zibcontabil-seqitem = wl_planilha-value.
      WHEN 3.
        wl_zibcontabil-bschl = wl_planilha-value.
      WHEN 4.
        wl_zibcontabil-gsber = wl_planilha-value.
      WHEN 5.
        wl_zibcontabil-bukrs = wl_planilha-value.
      WHEN 6.
        wl_zibcontabil-interface = wl_planilha-value.
      WHEN 7.
        wl_zibcontabil-bktxt = wl_planilha-value.
      WHEN 8.
        wl_zibcontabil-bldat = wl_planilha-value.
      WHEN 9.
        wl_zibcontabil-budat = wl_planilha-value.
      WHEN 10.
        wl_zibcontabil-gjahr = wl_planilha-value.
      WHEN 11.
        wl_zibcontabil-monat = wl_planilha-value.
      WHEN 12.
        wl_zibcontabil-blart = wl_planilha-value.
      WHEN 13.
        wl_zibcontabil-xblnr = wl_planilha-value.
      WHEN 14.
        wl_zibcontabil-hkont = wl_planilha-value.
      WHEN 15.
        PERFORM tratar_campo CHANGING wl_planilha-value.
        wl_zibcontabil-wrbtr = wl_planilha-value.
      WHEN 16.
        wl_zibcontabil-waers = wl_planilha-value.
      WHEN 17.
        wl_zibcontabil-zfbdt = wl_planilha-value.
      WHEN 18.
        wl_zibcontabil-zlspr = wl_planilha-value.
      WHEN 19.
        wl_zibcontabil-zlsch = wl_planilha-value.
      WHEN 20.
        wl_zibcontabil-kidno = wl_planilha-value.
      WHEN 21.
        wl_zibcontabil-sgtxt = wl_planilha-value.
      WHEN 22.
        wl_zibcontabil-xref1 = wl_planilha-value.
      WHEN 23.
        wl_zibcontabil-xref2 = wl_planilha-value.
      WHEN 24.
        wl_zibcontabil-xref3 = wl_planilha-value.
      WHEN 25.
        wl_zibcontabil-bupla = wl_planilha-value.
      WHEN 26.
        wl_zibcontabil-zuonr = wl_planilha-value.
      WHEN 27.
        wl_zibcontabil-umskz = wl_planilha-value.
      WHEN 28.
        wl_zibcontabil-kostl = wl_planilha-value.
      WHEN 29.
        wl_zibcontabil-aufnr = wl_planilha-value.
      WHEN 30.
        wl_zibcontabil-prctr = wl_planilha-value.
      WHEN 31.
        wl_zibcontabil-waers_i = wl_planilha-value.
      WHEN 32.
        PERFORM tratar_campo CHANGING wl_planilha-value.
        wl_zibcontabil-dmbtr = wl_planilha-value.
      WHEN 33.
        wl_zibcontabil-waers_f = wl_planilha-value.
      WHEN 34.
        PERFORM tratar_campo CHANGING wl_planilha-value.
        wl_zibcontabil-dmbe2 = wl_planilha-value.
      WHEN 35.
        wl_zibcontabil-bvtyp = wl_planilha-value.
      WHEN 36.
        wl_zibcontabil-hbkid = wl_planilha-value.
      WHEN 37.
        wl_zibcontabil-rg_atualizado = wl_planilha-value.
      WHEN 38.
        wl_zibcontabil-bankl = wl_planilha-value.
      WHEN 39.
        wl_zibcontabil-bankn = wl_planilha-value.
      WHEN 40.
        wl_zibcontabil-newbw = wl_planilha-value.
      WHEN 41.
        wl_zibcontabil-anln1 = wl_planilha-value.
      WHEN 42.
        wl_zibcontabil-anln2 = wl_planilha-value.
      WHEN 43.
        wl_zibcontabil-bewar = wl_planilha-value.
      WHEN 44.
        wl_zibcontabil-waers_g = wl_planilha-value.
      WHEN 45.
        PERFORM tratar_campo CHANGING wl_planilha-value.
        wl_zibcontabil-dmbe3 = wl_planilha-value.
      WHEN 46.
        wl_zibcontabil-vbund = wl_planilha-value.
      WHEN 48.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_planilha-value
          IMPORTING
            output = wl_zibcontabil-matnr.
      WHEN 50.
        PERFORM tratar_campo CHANGING wl_planilha-value.
        wl_zibcontabil-quantity = wl_planilha-value.

      WHEN 51.
        wl_zibcontabil-base_uom = wl_planilha-value.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = wl_planilha-value
*          IMPORTING
*            output = wl_zibcontabil-quantity.
      WHEN 53.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_planilha-value
          IMPORTING
            output = wl_zibcontabil-matnr_fi.
      WHEN 54.
        wl_zibcontabil-vornr = wl_planilha-value.
      WHEN 55.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_planilha-value
          IMPORTING
            output = wl_zibcontabil-vbeln.
      WHEN 56.
        wl_zibcontabil-rldnr = wl_planilha-value.

    ENDCASE.

    AT END OF row.
      APPEND wl_zibcontabil TO gt_zibcontabil.
    ENDAT.
  ENDLOOP.

  DELETE gt_zibcontabil WHERE obj_key IS INITIAL.
  MODIFY zib_contabil FROM TABLE gt_zibcontabil.
  COMMIT WORK.

  LOOP AT gt_zibcontabil INTO wl_zibcontabil.
    wl_saida-status  = icon_yellow_light.
    wl_saida-obj_key = wl_zibcontabil-obj_key.
    wl_saida-seqitem = wl_zibcontabil-seqitem.
    wl_saida-bukrs   = wl_zibcontabil-bukrs.
    APPEND wl_saida TO gt_saida.
  ENDLOOP.

  SORT gt_saida BY seqitem.
  MESSAGE TEXT-s02 TYPE 'I' DISPLAY LIKE 'S'.
ENDFORM.                    "TRATAR_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM alv_preenche_cat       USING: p_campo         TYPE c
                                   p_desc          TYPE c
                                   p_tam           TYPE c
                                   p_hot           TYPE c
                                   p_zero          TYPE c
                                   p_sum           TYPE c
                                   p_icon          TYPE c.
  DATA:
  wl_fcat TYPE lvc_s_fcat.

  wl_fcat-fieldname  = p_campo.
  wl_fcat-scrtext_l  = p_desc.
  wl_fcat-scrtext_m  = p_desc.
  wl_fcat-scrtext_s  = p_desc.
  wl_fcat-hotspot    = p_hot.
  wl_fcat-no_zero    = p_zero.
  wl_fcat-outputlen  = p_tam.
  wl_fcat-icon       = p_icon.

  APPEND wl_fcat TO gt_fcat.
ENDFORM.                    "ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  tratar_campo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tratar_campo CHANGING v_value.
*  REPLACe '.' WITH ' ' INTO V_VALUE.
*  REPLACE ',' WITH '.' INTO V_VALUE.
*  REPLACE '-' WITH ' ' INTO V_VALUE.

  TRANSLATE v_value USING '. '.
  TRANSLATE v_value USING ',.'.
  TRANSLATE v_value USING '- '.

  CONDENSE v_value NO-GAPS.
ENDFORM.                    "tratar_campo

*&---------------------------------------------------------------------*
*&      Form  show_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM show_msg .

  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen   = '100'
      i_show     = 'X'
      i_repid    = sy-repid
    IMPORTING
      e_messagem = wl_mensagem
    TABLES
      it_msgs    = gt_msg_return.
ENDFORM.                    "show_msg

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  REFRESH gt_fcat.
  PERFORM alv_preenche_cat USING:
    'STATUS'         'Status'         '6'   ''  ''  '' 'X',
    'OBJ_KEY'        'ObjKey'         '20'  ''  ''  '' '',
    'BUKRS'          'Empresa'        '10'  ''  ''  '' '',
    'SEQITEM'        'Seq Item'       '10'  ''  ''  '' '',
    'BELNR'          'Documento'      '15'  ''  ''  '' ''.
*    'DESCR'          'Descrição'      '75'  ''  ''  '' ''.

  IF ( obj_custom IS INITIAL ).
    CREATE OBJECT obj_custom
      EXPORTING
        container_name              = 'CUSTOM'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv
      EXPORTING
        i_parent          = obj_custom
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    wl_layout-zebra      = 'X'.
  ENDIF.

  CREATE OBJECT obj_splitter
    EXPORTING
      parent  = obj_custom
      rows    = 2
      columns = 1.

  CALL METHOD obj_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = obj_custom_log.

  SET HANDLER:
  lcl_event_toolbar=>set_toolbar     FOR obj_alv,
  lcl_event_toolbar=>get_ucomm       FOR obj_alv.

  CALL METHOD obj_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = wl_layout
      i_default                     = 'X'
      i_save                        = 'A'
    CHANGING
      it_outtab                     = gt_saida
      it_fieldcatalog               = gt_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD obj_splitter->set_row_height
    EXPORTING
      id     = 1
      height = 100.
ENDMODULE.                 " PBO_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BTN_EXECUTAR'.
      IF ( p_file IS INITIAL ).
        MESSAGE TEXT-e01 TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        CHECK ( gt_msg_return IS INITIAL ).
        PERFORM tratar_arquivo.
      ENDIF.
    WHEN 'SHOW_MSG'.
      PERFORM show_msg.
    WHEN OTHERS.
  ENDCASE.

  CLEAR sy-ucomm.
ENDMODULE.                 " PAI_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  CARREGA_ARQUIVO  INPUT
*&---------------------------------------------------------------------*
MODULE carrega_arquivo INPUT.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = p_file
      mask             = ',*.xlsx.'
      mode             = 'O'
      title            = 'Arquivo a importar'
    IMPORTING
      filename         = p_file
    EXCEPTIONS
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.

ENDMODULE.                 " CARREGA_ARQUIVO  INPUT
