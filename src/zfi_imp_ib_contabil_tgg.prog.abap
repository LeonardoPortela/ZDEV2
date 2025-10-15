
*&---------------------------------------------------------------------*
*& Report  ZFI_IMP_IB_CONTABIL_TGG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfi_imp_ib_contabil_tgg.

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

        MESSAGE s836(sd) WITH text-s01 DISPLAY LIKE 'S'.

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
  DATA: lva_count TYPE i.
  REFRESH: gt_planilha, gt_saida, gt_zibcontabil.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = text-i01.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 55
      i_end_row               = 10000
    TABLES
      intern                  = gt_planilha
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  CLEAR lva_count.
  LOOP AT gt_planilha INTO wl_planilha.
    AT NEW row.
      CLEAR wl_zibcontabil.
    ENDAT.

    IF wl_planilha-value(1) = space.
      SHIFT wl_planilha-value LEFT DELETING LEADING space.
    ENDIF.

    CASE wl_planilha-col.
      WHEN 1.
        wl_zibcontabil-obj_key = wl_planilha-value.
      WHEN 2.
        wl_zibcontabil-seqitem = wl_planilha-value.
      WHEN 3.
        wl_zibcontabil-bschl = wl_planilha-value.
      WHEN 4.
        wl_zibcontabil-gsber = wl_planilha-value.
      WHEN 5.
        wl_zibcontabil-bukrs = wl_planilha-value.
        IF wl_zibcontabil-bukrs <> '0048'.
          lva_count = lva_count + 1.
        ENDIF.
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

    ENDCASE.

    AT END OF row.
      APPEND wl_zibcontabil TO gt_zibcontabil.
    ENDAT.
  ENDLOOP.

  IF lva_count > 0.
    DELETE gt_zibcontabil WHERE bukrs = '0048'.
  ELSE.
    DELETE gt_zibcontabil WHERE obj_key IS INITIAL.

    LOOP AT gt_zibcontabil INTO wl_zibcontabil.
      IF ( wl_zibcontabil-obj_key  NE wl_zibcontabil_err-obj_key ).
        SELECT SINGLE *
          FROM zib_contabil_err
          INTO wl_zibcontabil_err
         WHERE obj_key = wl_zibcontabil-obj_key.

        IF ( sy-subrc IS INITIAL ).
          DELETE FROM zib_contabil_err WHERE obj_key = wl_zibcontabil-obj_key.
          DELETE FROM zib_contabil     WHERE obj_key = wl_zibcontabil-obj_key.
          COMMIT WORK.
        ENDIF.
      ENDIF.
      CLEAR: wl_zibcontabil.
    ENDLOOP.

    MODIFY zib_contabil FROM TABLE gt_zibcontabil.
    COMMIT WORK.
  ENDIF.

  IF  lva_count = 0.
    LOOP AT gt_zibcontabil INTO wl_zibcontabil.
      wl_saida-status  = icon_yellow_light.
      wl_saida-obj_key = wl_zibcontabil-obj_key.
      wl_saida-seqitem = wl_zibcontabil-seqitem.
      wl_saida-bukrs   = wl_zibcontabil-bukrs.
      APPEND wl_saida TO gt_saida.
    ENDLOOP.
    MESSAGE text-s02 TYPE 'I' DISPLAY LIKE 'S'.
  ELSE.
    LOOP AT gt_zibcontabil INTO wl_zibcontabil.
      wl_saida-status  = icon_red_light.
      wl_saida-obj_key = wl_zibcontabil-obj_key.
      wl_saida-seqitem = wl_zibcontabil-seqitem.
      wl_saida-bukrs   = wl_zibcontabil-bukrs.
      APPEND wl_saida TO gt_saida.
    ENDLOOP.
    MESSAGE text-s03 TYPE 'I' DISPLAY LIKE 'S'.
  ENDIF.

  SORT gt_saida BY seqitem.

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
        MESSAGE text-e01 TYPE 'I' DISPLAY LIKE 'E'.
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
