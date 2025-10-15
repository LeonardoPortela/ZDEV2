
*&---------------------------------------------------------------------*
*& Report  ZMMR185
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zmmr195.

*--------------------------------------------------------------------------------------------------*
* Developer: Anderson Oenning
* Data.....: 25.04.2023
*--------------------------------------------------------------------------------------------------*

TYPE-POOLS icon.

TYPES:
  BEGIN OF ty_saida,
    status       TYPE icon-id,
    material     TYPE bapimathead-material, "Código material
    mbrsh        TYPE mara-mbrsh,           "Setor industrial
    mtart        TYPE mara-mtart,           "Tipo de material
    matkl        TYPE mara-matkl,           "Grupo de mercadorias
    spart        TYPE mara-spart,           "Setor de atividade
    gewei        TYPE mara-gewei,           "Unidade de peso
    meins        TYPE mara-meins,           "Unidade de medida básica
    maktx        TYPE makt-maktx,           "Texto breve de material
    werks        TYPE marc-werks,           "Centro
    mtvfp        TYPE marc-mtvfp,           "Grupo de verificação para verificação de disponibilidade
    steuc        TYPE marc-steuc,           "Código de controle p/imposto seletivo em comércio exterior
    kautb        TYPE marc-kautb,           "Dados de centro para material
    bklas        TYPE mbew-bklas,           "Classe de avaliação
    mlast        TYPE mbew-mlast,           "Apropriação custos do ledger de materiais: controle
    peinh_1      TYPE mbew-peinh,           "Unidade preço
    mtuse        TYPE mbew-mtuse,           "Utilização de material
    mtorg        TYPE mbew-mtorg,           "Origem de material
    vkorg        TYPE tvko-vkorg,           "Organização de vendas
    vtweg        TYPE mvke-vtweg,           "Canal de distribuição
    sktof        TYPE mvke-sktof,           "Dados de venda para material
    mtpos        TYPE mvke-mtpos,           "Grupo de categorias de item do mestre de materiais
    message(255) TYPE c,
    check        TYPE c,
  END OF ty_saida.


DATA: obj_custom         TYPE REF TO cl_gui_custom_container,
      obj_custom_log     TYPE REF TO cl_gui_container,
      obj_splitter       TYPE REF TO cl_gui_splitter_container,
      ls_stable          TYPE lvc_s_stbl,
      obj_alv            TYPE REF TO cl_gui_alv_grid,
      gt_planilha        LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE,
      gt_planilha2       LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE,
      gt_msg_return      TYPE TABLE OF zfiwrs0002,
      gt_fcat            TYPE TABLE OF lvc_s_fcat,
      gt_saida           TYPE TABLE OF ty_saida,
      wa_saida           TYPE ty_saida,
      wl_planilha        LIKE alsmex_tabline,
      wl_msg_return      TYPE zfiwrs0002,
      wl_saida           TYPE ty_saida,
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


DATA: gs_headdata             TYPE bapimathead,
      gs_clientdata           TYPE bapi_mara,
      gs_clientdatax          TYPE bapi_marax,
      gs_forecastparameters   TYPE bapi_mpop,
      gs_forecastparametersx  TYPE bapi_mpopx,
      gs_mat_description      TYPE bapi_makt,
      gs_mat_longtext         TYPE bapi_mltx,
      gs_taxclassifications   TYPE bapi_mlan,
      gs_plantdata            TYPE bapi_marc,
      gs_plantdatax           TYPE bapi_marcx,
      gs_storagelocationdata  TYPE bapi_mard,
      gs_storagelocationdatax TYPE bapi_mardx,
      gs_storagetypedata      TYPE bapi_mlgt,
      gs_storagetypedatax     TYPE bapi_mlgtx,
      gs_salesdata            TYPE bapi_mvke,
      gs_salesdatax           TYPE bapi_mvkex,
      gs_unitsofmeasure       TYPE bapi_marm,
      gs_unitsofmeasurex      TYPE bapi_marmx,
      gs_valuationdata        TYPE bapi_mbew,
      gs_valuationdatax       TYPE bapi_mbewx,
      gs_return               TYPE bapiret2,
      gt_ret_messages         TYPE TABLE OF bapi_matreturn2.

DATA: v_matold      TYPE CHAR18.
DATA : i_matl_type  LIKE bapimatdoa-matl_type,   "Tipo de material
       i_ind_sector LIKE bapimatdoa-ind_sector,  "Setor industrial
       e_return_get LIKE bapireturn1.            "Return function

* Tabelas
DATA : t_material_number LIKE bapimatinr OCCURS 1 WITH HEADER LINE.



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
  REFRESH: gt_planilha, gt_saida.
  DATA: ws_ekpo TYPE ekpo.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = TEXT-i01.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 20
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
        wa_saida-mbrsh  = wl_planilha-value.
      WHEN 2.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-mtart = wl_planilha-value.
      WHEN 3.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-matkl = wl_planilha-value.
      WHEN 4.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-spart = wl_planilha-value.
      WHEN 5.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-gewei = wl_planilha-value.

      WHEN 6.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-meins = wl_planilha-value.

      WHEN 7.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-maktx = wl_planilha-value.
      WHEN 8.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-werks  = wl_planilha-value.
      WHEN 9.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-mtvfp = wl_planilha-value.
      WHEN 10.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-steuc = wl_planilha-value.

      WHEN 11.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-kautb  = wl_planilha-value.
      WHEN 12.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-bklas = wl_planilha-value.
      WHEN 13.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-mlast = wl_planilha-value.
      WHEN 14.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-peinh_1 = wl_planilha-value.
      WHEN 15.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-mtuse = wl_planilha-value.
      WHEN 16.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-mtorg = wl_planilha-value.
      WHEN 17.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-vkorg = wl_planilha-value.

      WHEN 18.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-vtweg = wl_planilha-value.

      WHEN 19.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-sktof = wl_planilha-value.
      WHEN 20.
        REPLACE ALL OCCURRENCES OF ',' IN wl_planilha-value WITH '.'.
        CONDENSE  wl_planilha-value NO-GAPS.
        wa_saida-mtpos = wl_planilha-value.

    ENDCASE.
    AT END OF row.
      APPEND wa_saida TO gt_saida.
    ENDAT.
  ENDLOOP.


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
    'STATUS     '  'Status proc                '   '6'   ''  ''  '' '',
    'material   '  'Código material            '   '10'  ''  ''  '' '' ,
    'mbrsh      '  'Setor industrial           '   '15'  ''  ''  '' '' ,
    'mtart      '  'Tipo de material           '   '15'  ''  ''  '' '',
    'matkl      '  'Grupo de mercadorias       '   '15'  ''  ''  '' '',
    'spart      '  'Setor de atividade         '   '15'  ''  ''  '' '' ,
    'gewei      '  'Unidade de peso            '   '15'  ''  ''  '' '' ,
    'meins      '  'Unidade de medida          '   '15'  ''  ''  '' '' ,
    'maktx      '  'Texto breve de material    '   '15'  ''  ''  '' '' ,
    'werks      '  'Centro                     '   '15'  ''  ''  '' '' ,
    'mtvfp      '  'Grupo ver disponibilidade  '   '15'  ''  ''  '' '' ,
    'steuc      '  'Código de controle         '   '15'  ''  ''  '' '' ,
    'kautb      '  'Dados de centro            '   '15'  ''  ''  '' '' ,
    'bklas      '  'Classe de avaliação        '   '15'  ''  ''  '' '' ,
    'mlast      '  'Apropriação c. ledger      '   '15'  ''  ''  '' '' ,
    'peinh_1    '  'Unidade preço              '   '15'  ''  ''  '' '' ,
    'mtuse      '  'Utilização de material     '   '15'  ''  ''  '' '' ,
    'mtorg      '  'Origem de material         '   '15'  ''  ''  '' '' ,
    'vkorg      '  'Organização de vendas      '   '15'  ''  ''  '' '' ,
    'vtweg      '  'Canal de distribuição      '   '15'  ''  ''  '' '' ,
    'sktof      '  'Dados de venda             '   '15'  ''  ''  '' '' ,
    'mtpos      '  'Grupo de categorias        '   '15'  ''  ''  '' '' ,
    'MESSAGE    '  'Mensagem                   '   '255' ''  ''  '' ''.

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

    LOOP AT gt_saida ASSIGNING FIELD-SYMBOL(<ls_saida>) WHERE check EQ abap_true.

      "Gera sequencia numero material.
      CALL FUNCTION 'BAPI_STDMATERIAL_GETINTNUMBER' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          material_type   = <ls_saida>-mtart
          industry_sector = <ls_saida>-mbrsh
        IMPORTING
          return          = e_return_get
        TABLES
          material_number = t_material_number.
      IF e_return_get-type NE 'S'.
        <ls_saida>-status  = icon_green_light.
        <ls_saida>-message = 'Documento processado com sucesso.'.
        CONTINUE.
      ELSE.
        READ TABLE t_material_number INDEX 1.
        v_matold = |{ t_material_number-material ALPHA = OUT }|.
        <ls_saida>-material = |{ v_matold ALPHA = IN }|.
      ENDIF.



      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          headdata       = gs_headdata
          clientdata     = gs_clientdata
          clientdatax    = gs_clientdatax
          plantdata      = gs_plantdata
          plantdatax     = gs_plantdatax
          valuationdata  = gs_valuationdata
          valuationdatax = gs_valuationdatax
        IMPORTING
          return         = gs_return
        TABLES
*         unitsofmeasure = gt_unitsofmeasure
*         unitsofmeasurex     = gt_unitsofmeasurex
*         materialdescription = gt_mat_description
*         materiallongtext    = gt_mat_longtext
          returnmessages = gt_ret_messages.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.



      <ls_saida>-status  = icon_green_light.
      <ls_saida>-message = 'Documento processado com sucesso.'.
    ENDLOOP.


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
