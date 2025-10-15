
*&---------------------------------------------------------------------*
*& Report  ZMMR185
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zppr025.

*--------------------------------------------------------------------------------------------------*
* Developer: Anderson Oenning
* Data.....: 25.04.2023
*--------------------------------------------------------------------------------------------------*

TYPE-POOLS icon.

TYPES:
  BEGIN OF ty_arq,
    docnum           TYPE j_1bnflin-docnum,
    zvlr_nf          TYPE j_1bnetpri,
    zvg_base_pis     TYPE j_1bnetpri,
    zvg_aliq_pis     TYPE j_1bnetpri,
    zvg_vlr_pis      TYPE j_1bnetpri,
    zvg_base_confins TYPE j_1bnetpri,
    zvg_aliq_confins TYPE j_1bnetpri,
    zvg_vlr_confins  TYPE j_1bnetpri,
  END OF ty_arq.

TYPES: BEGIN OF ty_saida,
         status             TYPE icon-id,
         docnum             TYPE j_1bnflin-docnum,
         zvlr_nf            TYPE j_1bnetpri,
         zvg_base_pis       TYPE j_1bnetpri,
         zvg_aliq_pis       TYPE j_1bnetpri,
         zvg_vlr_pis        TYPE j_1bnetpri,
         zvg_othbas_pis     TYPE j_1bnetpri,
         zvg_base_confins   TYPE j_1bnetpri,
         zvg_aliq_confins   TYPE j_1bnetpri,
         zvg_vlr_confins    TYPE j_1bnetpri,
         zvg_othbas_confins TYPE j_1bnetpri,
         zvg_base_icm       TYPE j_1bnetpri,
         zvg_aliq_icm       TYPE j_1bnetpri,
         zvg_vlr_icm        TYPE j_1bnetpri,
         zvg_othbas_icm     TYPE j_1bnetpri,
         message(255)       TYPE c,
         check              TYPE c,
       END OF ty_saida.

TYPES: BEGIN OF ty_saidar,
         status           TYPE icon-id,
         docnum           TYPE j_1bnflin-docnum,
         zvlr_nf          TYPE j_1bnetpri,
         zvg_base_pis     TYPE j_1bnetpri,
         zvg_aliq_pis     TYPE j_1bnetpri,
         zvg_vlr_pis      TYPE j_1bnetpri,
         zvg_base_confins TYPE j_1bnetpri,
         zvg_aliq_confins TYPE j_1bnetpri,
         zvg_vlr_confins  TYPE j_1bnetpri,
         message(255)     TYPE c,
       END OF ty_saidar.

DATA: obj_custom         TYPE REF TO cl_gui_custom_container,
      obj_custom_log     TYPE REF TO cl_gui_container,
      obj_splitter       TYPE REF TO cl_gui_splitter_container,
      ls_stable          TYPE lvc_s_stbl,
      obj_alv            TYPE REF TO cl_gui_alv_grid,
      gt_planilha        LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE,
      gt_planilha2       LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE,
      gt_msg_return      TYPE TABLE OF zfiwrs0002,
      gt_zibcontabil     TYPE TABLE OF zib_contabil,
      gt_zibcontabil_err TYPE TABLE OF zib_contabil_err,
      gt_fcat            TYPE TABLE OF lvc_s_fcat,
      gt_saida           TYPE TABLE OF ty_saida,
      gt_j_1bnfdoc       TYPE TABLE OF j_1bnfdoc,
      gt_j_1bnflin       TYPE TABLE OF j_1bnflin,
      gt_j_1bnfstx       TYPE TABLE OF j_1bnfstx,
      it_j_1bnflin       TYPE TABLE OF j_1bnflin,
      it_j_1bnfstx       TYPE TABLE OF j_1bnfstx,

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



DATA it_arq  TYPE TABLE OF ty_arq.


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
*
*    wl_toolbar-butn_type    = 1.
*    APPEND wl_toolbar TO e_object->mt_toolbar.
*    CLEAR wl_toolbar.
*
    IF gt_saida IS NOT INITIAL.
      wl_toolbar-function     = 'BTN_PROC'.
      wl_toolbar-icon         = icon_oo_interface.
      wl_toolbar-butn_type    = 0.
      wl_toolbar-text         = 'Processar'.
      APPEND wl_toolbar TO e_object->mt_toolbar.
      CLEAR wl_toolbar.
    ENDIF.
  ENDMETHOD.                    "SET_TOOLBAR

  METHOD get_ucomm.
    CASE e_ucomm.
      WHEN 'BTN_ATUALIZAR'.
        DATA: at_index TYPE sy-tabix.
        CLEAR: gt_msg_return.

        IF ( NOT gt_msg_return IS INITIAL ).
          PERFORM show_msg.
        ENDIF.

        MESSAGE s836(sd) WITH TEXT-s01 DISPLAY LIKE 'S'.

        CALL METHOD obj_alv->refresh_table_display
          EXPORTING
            is_stable = wl_stable.

      WHEN 'BTN_PROC'.
        PERFORM fm_proc_dados.

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
      text = TEXT-i01.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 15
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
        wa_saida-docnum = wl_planilha-value.
        wa_saida-docnum = |{ wa_saida-docnum  ALPHA = IN }|.
      WHEN 2.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-zvlr_nf = wl_planilha-value.
      WHEN 3.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-zvg_base_pis = wl_planilha-value.
      WHEN 4.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-zvg_aliq_pis = wl_planilha-value.
      WHEN 5.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-zvg_vlr_pis = wl_planilha-value.

      WHEN 6.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-zvg_othbas_pis = wl_planilha-value.

      WHEN 7.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-zvg_base_confins = wl_planilha-value.
      WHEN 8.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-zvg_aliq_confins = wl_planilha-value.
      WHEN 9.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-zvg_vlr_confins = wl_planilha-value.
      WHEN 10.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-zvg_othbas_confins = wl_planilha-value.

      WHEN 11.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-zvg_base_icm = wl_planilha-value.
      WHEN 12.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-zvg_aliq_icm = wl_planilha-value.
      WHEN 13.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-zvg_vlr_icm = wl_planilha-value.
      WHEN 14.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-zvg_othbas_icm = wl_planilha-value.
    ENDCASE.
    AT END OF row.
      APPEND wa_saida TO gt_saida.
    ENDAT.
  ENDLOOP.

  CHECK gt_saida IS NOT INITIAL.

  FREE: gt_j_1bnfdoc.
  SELECT *
  FROM j_1bnfdoc
  INTO CORRESPONDING FIELDS OF TABLE gt_j_1bnfdoc
  FOR ALL ENTRIES IN gt_saida
  WHERE docnum EQ gt_saida-docnum.

  LOOP AT gt_saida ASSIGNING FIELD-SYMBOL(<ws_saida>).

    <ws_saida>-docnum = |{ <ws_saida>-docnum ALPHA = IN }|.

    READ TABLE gt_j_1bnfdoc INTO DATA(wa_fdoc) WITH KEY docnum = <ws_saida>-docnum.
    IF sy-subrc EQ 0.
      <ws_saida>-status = icon_generate.
      <ws_saida>-message = 'Documento  validado aguardando processamento'.
      <ws_saida>-check   = abap_true.
      CONTINUE.
    ELSE.
      <ws_saida>-status = icon_led_red.
      <ws_saida>-message = 'Documento não encontrado'.
    ENDIF.

  ENDLOOP.


  CLEAR: wa_saida.
*  MESSAGE text-s02 TYPE 'I' DISPLAY LIKE 'S'.
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
    'STATUS            '  'Status proc                     '   '6'   ''  ''  '' '',
    'DOCNUM            '  'Nº Doc                          '   '10'  ''  ''  '' '' ,
    'ZVLR_NF           '  'Valor Total NF Correto (SAP)    '   '15'  ''  ''  '' '' ,

    'ZVG_BASE_PIS      '  'Base PIS (Correto)              '   '15'  ''  ''  '' '',
    'ZVG_ALIQ_PIS      '  'Alíquota PIS (Correto)          '   '15'  ''  ''  '' '',
    'ZVG_VLR_PIS       '  'Valor PIS (Correto)             '   '15'  ''  ''  '' '' ,
    'ZVG_OTHBAS_PIS    '  'Outros imposto PIS (Correto)    '   '15'  ''  ''  '' '' ,

    'ZVG_BASE_CONFINS  '  'Base COFINS (Correto)           '   '15'  ''  ''  '' '' ,
    'ZVG_ALIQ_CONFINS  '  'Alíquota COFINS (Correto)       '   '15'  ''  ''  '' '' ,
    'ZVG_VLR_CONFINS   '  'Valor COFINS (Correto)          '   '15'  ''  ''  '' '' ,
    'ZVG_OTHBAS_CONFINS'  'Outros imposto COFINS (Correto) '   '15'  ''  ''  '' '' ,

    'ZVG_BASE_ICM      '  'Base ICM1 (Correto)             '   '15'  ''  ''  '' '' ,
    'ZVG_ALIQ_ICM      '  'Alíquota ICM1 (Correto)         '   '15'  ''  ''  '' '' ,
    'ZVG_VLR_ICM       '  'Valor ICM1 (Correto)            '   '15'  ''  ''  '' '' ,
    'ZVG_OTHBAS_ICM    '  'Outros imposto ICM1 (Correto)   '   '15'  ''  ''  '' '' ,

    'MESSAGE           '  'Mensagem                        '   '255' ''  ''  '' ''.

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

  wl_layout-cwidth_opt = abap_true.
  wl_layout-zebra      = abap_true.


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

  CALL METHOD obj_alv->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.
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
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_PROC_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_proc_dados .

  DATA: w_answer(1).


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = TEXT-m04
*     TEXT_BUTTON_1         = 'Sim'(100)
      text_button_1         = TEXT-b01
      icon_button_1         = 'ICON_OKAY'
*     TEXT_BUTTON_2         = 'Não'(101)
      text_button_2         = TEXT-b02
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


  IF w_answer EQ 1.

    SORT gt_saida BY check.
    DELETE gt_saida WHERE check NE abap_true.

    CHECK gt_saida IS NOT INITIAL.

    "Selecionando informação tabela J_1BNFLIN.
    FREE: gt_j_1bnflin.
    SELECT * FROM j_1bnflin
      INTO CORRESPONDING FIELDS OF TABLE gt_j_1bnflin
      FOR ALL ENTRIES IN gt_saida
      WHERE docnum EQ gt_saida-docnum.

    "Selecionando informação tabela j_1bnfstx.
    FREE: gt_j_1bnfstx.
    SELECT * FROM j_1bnfstx
      INTO CORRESPONDING FIELDS OF TABLE gt_j_1bnfstx
      FOR ALL ENTRIES IN gt_saida
      WHERE docnum EQ gt_saida-docnum.

    "Selecionando informação tabela J_1BNFDOC.
    FREE: gt_j_1bnfdoc.
    SELECT * FROM j_1bnfdoc
      INTO CORRESPONDING FIELDS OF TABLE gt_j_1bnfdoc
      FOR ALL ENTRIES IN gt_saida
      WHERE docnum EQ gt_saida-docnum.

    LOOP AT gt_saida ASSIGNING FIELD-SYMBOL(<ls_saida>) WHERE check EQ abap_true.

      "Informações tabela J_1BNFLIN
      READ TABLE gt_j_1bnflin INTO DATA(ws_j_1bnflin) WITH KEY docnum = <ls_saida>-docnum.
      IF sy-subrc EQ 0.
        ws_j_1bnflin-netwr  = <ls_saida>-zvlr_nf.
        ws_j_1bnflin-nfnet  = <ls_saida>-zvlr_nf.
        ws_j_1bnflin-netwrt = <ls_saida>-zvlr_nf.
        ws_j_1bnflin-nfnett = <ls_saida>-zvlr_nf.
        APPEND ws_j_1bnflin TO it_j_1bnflin.
        CLEAR: ws_j_1bnflin.

      ELSE.
        CONTINUE.
      ENDIF.

      "Informações tabela j_1bnfstx
      LOOP AT gt_j_1bnfstx INTO DATA(ws_j_1bnfstx) WHERE docnum = <ls_saida>-docnum.


        CASE ws_j_1bnfstx-taxtyp.
          WHEN 'ICM0'.
            IF ws_j_1bnfstx-base IS NOT INITIAL AND <ls_saida>-zvg_base_icm IS NOT INITIAL.
              ws_j_1bnfstx-base   = <ls_saida>-zvg_base_icm.
            ENDIF.

            IF ws_j_1bnfstx-rate IS NOT INITIAL AND <ls_saida>-zvg_aliq_icm IS NOT INITIAL.
              ws_j_1bnfstx-rate   = <ls_saida>-zvg_aliq_icm.
            ENDIF.

            IF ws_j_1bnfstx-taxval IS NOT INITIAL AND <ls_saida>-zvg_vlr_icm IS NOT INITIAL.
              ws_j_1bnfstx-taxval = <ls_saida>-zvg_vlr_icm.
            ENDIF.

            IF ws_j_1bnfstx-othbas IS NOT INITIAL AND <ls_saida>-zvg_othbas_icm IS NOT INITIAL.
              ws_j_1bnfstx-othbas = <ls_saida>-zvg_othbas_icm.
            ENDIF.

          WHEN 'ICM1'.
            IF ws_j_1bnfstx-base IS NOT INITIAL AND <ls_saida>-zvg_base_icm IS NOT INITIAL.
              ws_j_1bnfstx-base   = <ls_saida>-zvg_base_icm.
            ENDIF.

            IF ws_j_1bnfstx-rate IS NOT INITIAL AND <ls_saida>-zvg_aliq_icm IS NOT INITIAL.
              ws_j_1bnfstx-rate   = <ls_saida>-zvg_aliq_icm.
            ENDIF.

            IF ws_j_1bnfstx-taxval IS NOT INITIAL AND <ls_saida>-zvg_vlr_icm IS NOT INITIAL.
              ws_j_1bnfstx-taxval = <ls_saida>-zvg_vlr_icm.
            ENDIF.

            IF ws_j_1bnfstx-othbas IS NOT INITIAL AND <ls_saida>-zvg_othbas_icm IS NOT INITIAL.
              ws_j_1bnfstx-othbas = <ls_saida>-zvg_othbas_icm.
            ENDIF.

          WHEN 'ICM2'.
            IF ws_j_1bnfstx-base IS NOT INITIAL AND <ls_saida>-zvg_base_icm IS NOT INITIAL.
              ws_j_1bnfstx-base   = <ls_saida>-zvg_base_icm.
            ENDIF.

            IF ws_j_1bnfstx-rate IS NOT INITIAL AND <ls_saida>-zvg_aliq_icm IS NOT INITIAL.
              ws_j_1bnfstx-rate   = <ls_saida>-zvg_aliq_icm.
            ENDIF.

            IF ws_j_1bnfstx-taxval IS NOT INITIAL AND <ls_saida>-zvg_vlr_icm IS NOT INITIAL.
              ws_j_1bnfstx-taxval = <ls_saida>-zvg_vlr_icm.
            ENDIF.

            IF ws_j_1bnfstx-othbas IS NOT INITIAL AND <ls_saida>-zvg_othbas_icm IS NOT INITIAL.
              ws_j_1bnfstx-othbas = <ls_saida>-zvg_othbas_icm.
            ENDIF.

          WHEN 'ICM3'.
            IF ws_j_1bnfstx-base IS NOT INITIAL AND <ls_saida>-zvg_base_icm IS NOT INITIAL.
              ws_j_1bnfstx-base   = <ls_saida>-zvg_base_icm.
            ENDIF.

            IF ws_j_1bnfstx-rate IS NOT INITIAL AND <ls_saida>-zvg_aliq_icm IS NOT INITIAL.
              ws_j_1bnfstx-rate   = <ls_saida>-zvg_aliq_icm.
            ENDIF.

            IF ws_j_1bnfstx-taxval IS NOT INITIAL AND <ls_saida>-zvg_vlr_icm IS NOT INITIAL.
              ws_j_1bnfstx-taxval = <ls_saida>-zvg_vlr_icm.
            ENDIF.

            IF ws_j_1bnfstx-othbas IS NOT INITIAL AND <ls_saida>-zvg_othbas_icm IS NOT INITIAL.
              ws_j_1bnfstx-othbas = <ls_saida>-zvg_othbas_icm.
            ENDIF.

          WHEN 'ICM4'.
            IF ws_j_1bnfstx-base IS NOT INITIAL AND <ls_saida>-zvg_base_icm IS NOT INITIAL.
              ws_j_1bnfstx-base   = <ls_saida>-zvg_base_icm.
            ENDIF.

            IF ws_j_1bnfstx-rate IS NOT INITIAL AND <ls_saida>-zvg_aliq_icm IS NOT INITIAL.
              ws_j_1bnfstx-rate   = <ls_saida>-zvg_aliq_icm.
            ENDIF.

            IF ws_j_1bnfstx-taxval IS NOT INITIAL AND <ls_saida>-zvg_vlr_icm IS NOT INITIAL.
              ws_j_1bnfstx-taxval = <ls_saida>-zvg_vlr_icm.
            ENDIF.

            IF ws_j_1bnfstx-othbas IS NOT INITIAL AND <ls_saida>-zvg_othbas_icm IS NOT INITIAL.
              ws_j_1bnfstx-othbas = <ls_saida>-zvg_othbas_icm.
            ENDIF.


          WHEN 'ICOF'. "Imposto Confins
            IF ws_j_1bnfstx-base IS NOT INITIAL OR <ls_saida>-zvg_base_confins IS NOT INITIAL.
              ws_j_1bnfstx-base   = <ls_saida>-zvg_base_confins.
            ENDIF.

            IF ws_j_1bnfstx-rate IS NOT INITIAL OR <ls_saida>-zvg_aliq_confins IS NOT INITIAL.
              ws_j_1bnfstx-rate   = <ls_saida>-zvg_aliq_confins.
            ENDIF.

            IF ws_j_1bnfstx-taxval IS NOT INITIAL OR <ls_saida>-zvg_vlr_confins IS NOT INITIAL.
              ws_j_1bnfstx-taxval = <ls_saida>-zvg_vlr_confins.
            ENDIF.

            IF ws_j_1bnfstx-othbas IS NOT INITIAL OR <ls_saida>-zvg_othbas_confins IS NOT INITIAL.
              ws_j_1bnfstx-taxval = <ls_saida>-zvg_othbas_confins.
            ENDIF.
          WHEN 'IPI0'.
            IF ws_j_1bnfstx-othbas IS NOT INITIAL AND <ls_saida>-zvg_base_confins IS NOT INITIAL.
              ws_j_1bnfstx-othbas = <ls_saida>-zvg_base_confins.
            ENDIF.
          WHEN 'IPIS'. "Imposto IPIS
            IF ws_j_1bnfstx-base IS NOT INITIAL AND <ls_saida>-zvg_base_pis IS NOT INITIAL.
              ws_j_1bnfstx-base   = <ls_saida>-zvg_base_pis.
            ENDIF.

            IF ws_j_1bnfstx-rate IS NOT INITIAL AND <ls_saida>-zvg_aliq_pis IS NOT INITIAL.
              ws_j_1bnfstx-rate   = <ls_saida>-zvg_aliq_pis.
            ENDIF.

            IF ws_j_1bnfstx-taxval IS NOT INITIAL AND <ls_saida>-zvg_vlr_pis IS NOT INITIAL.
              ws_j_1bnfstx-taxval = <ls_saida>-zvg_vlr_pis.
            ENDIF.

            IF ws_j_1bnfstx-othbas IS NOT INITIAL AND <ls_saida>-zvg_othbas_pis IS NOT INITIAL.
              ws_j_1bnfstx-taxval = <ls_saida>-zvg_othbas_pis.
            ENDIF.

          WHEN OTHERS.
        ENDCASE.

        APPEND ws_j_1bnfstx TO it_j_1bnfstx.
        CLEAR: ws_j_1bnfstx.
      ENDLOOP.

      <ls_saida>-status  = icon_green_light.
      <ls_saida>-message = 'Documento processado com sucesso.'.
    ENDLOOP.

    IF it_j_1bnflin IS NOT INITIAL AND it_j_1bnfstx IS NOT INITIAL.
      MODIFY j_1bnflin FROM TABLE it_j_1bnflin.
      MODIFY j_1bnfstx FROM TABLE it_j_1bnfstx.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ENDIF.
  ENDIF.

  CALL METHOD obj_alv->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

  IF sy-subrc <> 0.
  ENDIF.


ENDFORM.
