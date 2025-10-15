
*&---------------------------------------------------------------------*
*& Report  ZFI_IMP_IB_CONTABIL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zmmr184.

*--------------------------------------------------------------------------------------------------*
* Developer: Anderson Oenning
* Data.....: 25.04.2023
*--------------------------------------------------------------------------------------------------*

TYPE-POOLS icon.

TYPES:
  BEGIN OF ty_ekbe,
    ebeln TYPE ekbe-ebeln,
    ebelp TYPE ekbe-ebelp,
    menge TYPE ekbe-menge,
    dmbtr TYPE ekbe-dmbtr,
    shkzg TYPE ekbe-shkzg,
  END OF ty_ekbe.

TYPES: BEGIN OF ty_saida,
         status       TYPE icon-id,
         ebeln        TYPE ebeln,
         ebelp        TYPE ebelp,
         message(255) TYPE c,
       END OF ty_saida.

TYPES: BEGIN OF ty_saidar,
         status       TYPE icon-id,
         banfn        TYPE banfn,
         bnfpo        TYPE bnfpo,
         message(255) TYPE c,
       END OF ty_saidar.

DATA: obj_custom         TYPE REF TO cl_gui_custom_container,
      obj_custom_log     TYPE REF TO cl_gui_container,
      obj_splitter       TYPE REF TO cl_gui_splitter_container,
      obj_alv            TYPE REF TO cl_gui_alv_grid,
      gt_planilha        LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE,
      gt_planilha2       LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE,
      gt_msg_return      TYPE TABLE OF zfiwrs0002,
      gt_zibcontabil     TYPE TABLE OF zib_contabil,
      gt_zibcontabil_err TYPE TABLE OF zib_contabil_err,
      gt_fcat            TYPE TABLE OF lvc_s_fcat,
      gt_saida           TYPE TABLE OF ty_saida,
      wa_saida           TYPE ty_saida,
      gt_saidar          TYPE TABLE OF ty_saidar,
      wa_saidar          TYPE ty_saidar,
      wl_planilha        LIKE alsmex_tabline,
      wl_msg_return      TYPE zfiwrs0002,
      wl_saida           TYPE ty_saida,
      wl_saidar          TYPE ty_saidar,
      wl_zibcontabil     TYPE zib_contabil,
      wl_layout          TYPE lvc_s_layo,
      wl_mensagem        TYPE char30,
      wl_stable          TYPE lvc_s_stbl,
      wl_zibcontabil_chv TYPE zib_contabil_chv,
      wl_zibcontabil_err TYPE zib_contabil_err,
      wl_toolbar         TYPE stb_button,
      ok_code            LIKE sy-ucomm,
      vg_ped,
      vg_req,
      p_file             TYPE rlgrap-filename.

DATA it_ekpo TYPE TABLE OF ekpo.
DATA wa_ekpo TYPE ekpo.
DATA it_eban TYPE TABLE OF eban.
DATA wa_eban TYPE eban.
DATA it_ekbe  TYPE TABLE OF ty_ekbe.
DATA it_ekbem TYPE TABLE OF ty_ekbe.
DATA it_aufk  TYPE TABLE OF aufk.

DATA: t_return  LIKE bapiret2      OCCURS 0 WITH HEADER LINE.
DATA: t_item    LIKE bapimepoitem  OCCURS 0 WITH HEADER LINE.
DATA: t_itemx   LIKE bapimepoitemx OCCURS 0 WITH HEADER LINE.

DATA: t_itemr   TYPE TABLE OF bapimereqitemimp WITH HEADER LINE, " RJF
      t_itemxr  TYPE TABLE OF bapimereqitemx   WITH HEADER LINE,
      t_returnr TYPE TABLE OF bapiret2         WITH HEADER LINE.

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

*    wl_toolbar-butn_type    = 3.
*    APPEND wl_toolbar TO e_object->mt_toolbar.
*    CLEAR wl_toolbar.
*
*    wl_toolbar-function     = 'BTN_ATUALIZAR'.
*    wl_toolbar-icon         = icon_refresh.
*    wl_toolbar-butn_type    = 0.
*    wl_toolbar-text         = 'Atualizar'.
*
*    APPEND wl_toolbar TO e_object->mt_toolbar.
*    CLEAR wl_toolbar.
  ENDMETHOD.                    "SET_TOOLBAR

  METHOD get_ucomm.
    CASE e_ucomm.
      WHEN 'BTN_ATUALIZAR'.
        DATA: at_index TYPE sy-tabix.
        CLEAR: gt_msg_return.

        CASE abap_true.
          WHEN vg_ped.

            FREE: it_ekpo.
            SELECT *
            FROM ekpo
            INNER JOIN eket
            ON  eket~ebeln = ekpo~ebeln
            AND eket~ebelp = ekpo~ebelp
            INTO CORRESPONDING FIELDS OF TABLE it_ekpo
            FOR ALL ENTRIES IN gt_saida
            WHERE ekpo~ebeln EQ gt_saida-ebeln
            AND   ekpo~ebelp EQ gt_saida-ebelp
            AND   ekpo~elikz EQ ''   "Remessa final desmarcada
            AND   ekpo~loekz EQ ''.

            SORT it_ekpo BY ebeln ebelp.
            SORT gt_saida BY ebeln ebelp.

            LOOP AT gt_saida INTO wl_saida.

              at_index = sy-tabix.

              wl_saida-ebelp = |{ wl_saida-ebelp ALPHA = IN }|.

              CLEAR: wa_ekpo.
              READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wl_saida-ebeln ebelp = wl_saida-ebelp BINARY SEARCH.
              IF sy-subrc NE 0.
                wl_saida-status = icon_red_light.
                wl_saida-message = 'Documento  não localizado'.
              ENDIF.

              MODIFY gt_saida FROM wl_saida INDEX sy-tabix TRANSPORTING status message.

            ENDLOOP.

            IF ( NOT gt_msg_return IS INITIAL ).
              PERFORM show_msg.
            ENDIF.

            MESSAGE s836(sd) WITH text-s01 DISPLAY LIKE 'S'.

            CALL METHOD obj_alv->refresh_table_display
              EXPORTING
                is_stable = wl_stable.

          WHEN vg_req. "RJF

            FREE: it_eban.
            SELECT *
            FROM eban
*            INNER JOIN eket
*            ON  eket~ebeln = ekpo~ebeln
*            AND eket~ebelp = ekpo~ebelp
            INTO CORRESPONDING FIELDS OF TABLE it_eban
            FOR ALL ENTRIES IN gt_saidar
            WHERE banfn EQ gt_saidar-banfn
            AND   bnfpo EQ gt_saidar-bnfpo
            AND   ebakz EQ ''.

*banfn
*bnfpo
            SORT it_eban BY banfn bnfpo.
            SORT gt_saidar BY banfn bnfpo.

            LOOP AT gt_saidar INTO wl_saidar.

              at_index = sy-tabix.

              wl_saidar-bnfpo = |{ wl_saidar-bnfpo ALPHA = IN }|.

              CLEAR: wa_eban.
              READ TABLE it_eban INTO wa_eban WITH KEY banfn = wl_saidar-banfn ebelp = wl_saidar-bnfpo BINARY SEARCH.
              IF sy-subrc NE 0.
                wl_saidar-status = icon_red_light.
                wl_saidar-message = 'Documento  não localizado'.
              ENDIF.

              MODIFY gt_saidar FROM wl_saidar INDEX sy-tabix TRANSPORTING status message.

            ENDLOOP.

        ENDCASE.

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
  REFRESH: gt_planilha, gt_saida, gt_zibcontabil.
  DATA: ws_ekpo TYPE ekpo.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = text-i01.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 2
      i_end_row               = 10000
    TABLES
      intern                  = gt_planilha
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  LOOP AT gt_planilha INTO wl_planilha.
    AT NEW row.
      CLEAR wl_saida.
    ENDAT.

    IF wl_planilha-value(1) = space.
      SHIFT wl_planilha-value LEFT DELETING LEADING space.
    ENDIF.

    CASE wl_planilha-col.
      WHEN 1.
        wa_saida-ebeln = wl_planilha-value.
        wa_saida-ebeln = |{ wa_saida-ebeln  ALPHA = IN }|.

      WHEN 2.
        wa_saida-ebelp = wl_planilha-value.
        wa_saida-ebelp = |{ wa_saida-ebelp  ALPHA = IN }|.

    ENDCASE.

    AT END OF row.
      APPEND wa_saida TO gt_saida.
    ENDAT.
  ENDLOOP.


  CHECK gt_saida IS NOT INITIAL.

  FREE: it_ekpo.
  SELECT *
  FROM ekpo
  INNER JOIN eket
  ON  eket~ebeln = ekpo~ebeln
  AND eket~ebelp = ekpo~ebelp
  INTO CORRESPONDING FIELDS OF TABLE it_ekpo
  FOR ALL ENTRIES IN gt_saida
  WHERE ekpo~ebeln EQ gt_saida-ebeln
  AND   ekpo~ebelp EQ gt_saida-ebelp
  AND   ekpo~elikz EQ ''   "Remessa final desmarcada
  AND   ekpo~loekz EQ ''.

  SORT it_ekpo BY ebeln ebelp.
  SORT gt_saida BY ebeln ebelp.

  LOOP AT gt_saida ASSIGNING FIELD-SYMBOL(<ws_saida>).

    <ws_saida>-ebelp = |{ <ws_saida>-ebelp ALPHA = IN }|.

    CLEAR: wa_ekpo.
    READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = <ws_saida>-ebeln ebelp = <ws_saida>-ebelp BINARY SEARCH.
    IF sy-subrc NE 0.
      <ws_saida>-status = icon_red_light.
      <ws_saida>-message = 'Documento  não localizado'.
      CONTINUE.
    ENDIF.

    CLEAR: t_item, t_itemx,t_return.
    t_item-po_item    = wa_ekpo-ebelp.
    t_item-material   = wa_ekpo-matnr.
    t_item-matl_group = wa_ekpo-matkl.
*    t_item-pricedate  = wa_ekpo-prdat.
    t_item-no_more_gr = 'X'.
    APPEND t_item.


*    t_itemx-material   = 'X'.
*    t_itemx-matl_group = 'X'.
*    t_itemx-pricedate  = 'X'.
    t_itemx-po_item = wa_ekpo-ebelp.
    t_itemx-no_more_gr = 'X'.
    APPEND t_itemx.
    "
    CALL FUNCTION 'BAPI_PO_CHANGE'
      EXPORTING
        purchaseorder = wa_ekpo-ebeln
      TABLES
        return        = t_return
        poitem        = t_item
        poitemx       = t_itemx.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    WAIT UP TO 1 SECONDS.
    LOOP AT t_return.
      IF t_return-type = 'E'.
        <ws_saida>-status = icon_red_light.
        <ws_saida>-message = t_return-message.
        UPDATE  ekpo SET  elikz = 'X'
        WHERE  ebeln = wa_ekpo-ebeln
        AND    ebelp = wa_ekpo-ebelp.
        COMMIT WORK.
        CONCATENATE 'Forçado-'<ws_saida>-message  INTO <ws_saida>-message.
        EXIT.
      ELSE.
        <ws_saida>-status = icon_green_light.
        <ws_saida>-message = 'Processado com sucesso'.
      ENDIF.
    ENDLOOP.

    FREE: t_itemx[], t_item[], t_return[].

  ENDLOOP.


  CLEAR: wa_saida.
  MESSAGE text-s02 TYPE 'I' DISPLAY LIKE 'S'.
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

  CASE abap_true.
    WHEN vg_ped.

      PERFORM alv_preenche_cat USING:
        'STATUS '        'Status proc'         '6'   ''  ''  '' 'X',
        'EBELN  '        'Pedido     '         '20'  ''  ''  '' '',
        'EBELP  '        'Item       '         '10'  ''  ''  '' '',
        'MESSAGE'        'Mensagem   '         '255'  ''  ''  '' ''.

    WHEN vg_req. "RJF

      PERFORM alv_preenche_cat USING:
        'STATUS '        'Status proc'         '6'   ''  ''  '' 'X',
        'BANFN  '        'Requisição '         '20'  ''  ''  '' '',
        'BNFPO  '        'Item       '         '10'  ''  ''  '' '',
        'MESSAGE'        'Mensagem   '         '255'  ''  ''  '' ''.
  ENDCASE.


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

  CASE abap_true.
    WHEN vg_ped.

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

    WHEN vg_req.

      CALL METHOD obj_alv->set_table_for_first_display
        EXPORTING
          is_layout                     = wl_layout
          i_default                     = 'X'
          i_save                        = 'A'
        CHANGING
          it_outtab                     = gt_saidar
          it_fieldcatalog               = gt_fcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

  ENDCASE.

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
      CASE abap_true.
        WHEN vg_ped.
          IF ( p_file IS INITIAL ).
            MESSAGE text-e01 TYPE 'I' DISPLAY LIKE 'E'.
          ELSE.
            CHECK ( gt_msg_return IS INITIAL ).
            PERFORM tratar_arquivo.
          ENDIF.
        WHEN vg_req. "RJF
          IF ( p_file IS INITIAL ).
            MESSAGE text-e01 TYPE 'I' DISPLAY LIKE 'E'.
          ELSE.
            CHECK ( gt_msg_return IS INITIAL ).
            PERFORM tratar_arquivo_req.
            IF gt_saidar IS INITIAL AND gt_planilha IS INITIAL.
              MESSAGE 'Verificar conteúdo do arquivo!' TYPE 'I' DISPLAY LIKE 'E'.
            ELSE.
              CLEAR: wa_saidar.
              MESSAGE text-s02 TYPE 'I' DISPLAY LIKE 'S'.
            ENDIF.
          ENDIF.
      ENDCASE.
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
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  TRATAR_ARQUIVO_REQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tratar_arquivo_req .
  REFRESH: gt_planilha, gt_saidar, gt_zibcontabil.
  DATA: ws_eban TYPE eban.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = text-i02.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 2
      i_end_row               = 10000
    TABLES
      intern                  = gt_planilha
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  LOOP AT gt_planilha INTO wl_planilha.
    AT NEW row.
      CLEAR wl_saidar.
    ENDAT.

    IF wl_planilha-value(1) = space.
      SHIFT wl_planilha-value LEFT DELETING LEADING space.
    ENDIF.

    CASE wl_planilha-col.
      WHEN 1.
        wa_saidar-banfn = wl_planilha-value.
        wa_saidar-banfn = |{ wa_saidar-banfn  ALPHA = IN }|.

      WHEN 2.
        wa_saidar-bnfpo = wl_planilha-value.
        wa_saidar-bnfpo = |{ wa_saidar-bnfpo  ALPHA = IN }|.
    ENDCASE.

    AT END OF row.
      APPEND wa_saidar TO gt_saidar.
    ENDAT.
  ENDLOOP.

  CHECK gt_saidar IS NOT INITIAL.

  FREE: it_eban.
  SELECT *
  FROM eban
  INTO CORRESPONDING FIELDS OF TABLE it_eban
  FOR ALL ENTRIES IN gt_saidar
  WHERE banfn EQ gt_saidar-banfn
  AND   bnfpo EQ gt_saidar-bnfpo.

  SORT it_eban BY banfn bnfpo.
  SORT gt_saidar BY banfn bnfpo.

  LOOP AT gt_saidar ASSIGNING FIELD-SYMBOL(<ws_saida>).

    <ws_saida>-bnfpo = |{ <ws_saida>-bnfpo ALPHA = IN }|.

    CLEAR: wa_eban.
    READ TABLE it_eban INTO wa_eban WITH KEY banfn = <ws_saida>-banfn bnfpo = <ws_saida>-bnfpo BINARY SEARCH.
    IF sy-subrc NE 0.
      <ws_saida>-status = icon_red_light.
      <ws_saida>-message = 'Documento  não localizado'.
      CONTINUE.
    ENDIF.

*----------------------------------------------------

    CLEAR: t_itemr, t_returnr.
    t_itemr-preq_item   = wa_eban-bnfpo.
    t_itemr-closed      = abap_true.
    APPEND t_itemr.

    t_itemxr-preq_item  = wa_eban-bnfpo.
    t_itemxr-closed     = abap_true.
    APPEND t_itemxr.

*
    CALL FUNCTION 'BAPI_PR_CHANGE'
      EXPORTING
        number  = wa_eban-banfn
      TABLES
        return  = t_returnr
        pritem  = t_itemr
        pritemx = t_itemxr.

    CLEAR : t_returnr.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    WAIT UP TO 1 SECONDS.
    LOOP AT t_returnr.
      IF t_returnr-type = 'E'.
        <ws_saida>-status = icon_red_light.
        <ws_saida>-message = t_returnr-message.
        UPDATE  eban SET  ebakz = abap_true
        WHERE  banfn = wa_eban-banfn
        AND    bnfpo = wa_eban-bnfpo.

        COMMIT WORK.
        CONCATENATE 'Forçado-'<ws_saida>-message  INTO <ws_saida>-message.
        EXIT.
      ELSE.
* Banfn - Bnfpo
        SELECT SINGLE knttp, ebakz
        FROM eban
        INTO @DATA(wa_ebanx)
        WHERE banfn EQ @wa_eban-banfn
        AND   bnfpo EQ @wa_eban-bnfpo.
        IF sy-subrc EQ 0 AND ( wa_ebanx-knttp NE 'F' OR wa_ebanx-ebakz NE 'X' ).

          <ws_saida>-status = icon_red_light.
          <ws_saida>-message = t_returnr-message.
          UPDATE  eban SET  ebakz = abap_true
          WHERE  banfn = wa_eban-banfn
          AND    bnfpo = wa_eban-bnfpo.

          COMMIT WORK.
          CONCATENATE 'Forçado-'<ws_saida>-message  INTO <ws_saida>-message.
          EXIT.
        ENDIF.

        <ws_saida>-status = icon_green_light.
        <ws_saida>-message = 'Processado com sucesso'.
      ENDIF.
    ENDLOOP.
    FREE: t_itemr[], t_returnr[].

  ENDLOOP.

ENDFORM.
