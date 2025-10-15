FUNCTION-POOL zgsd_baixa_compra.            "MESSAGE-ID ..

****************************************************************
* TABELAS
****************************************************************
TABLES: zsdt0276,
        zsdt0277.

****************************************************************
* Definicao / implementacao classes
****************************************************************
CLASS lcl_simple_text_editor DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:   co_x  TYPE xfeld VALUE 'X'.

    DATA: container    TYPE REF TO cl_gui_custom_container
                                                    READ-ONLY,
          display_mode TYPE xfeld                   READ-ONLY,
          editor       TYPE REF TO cl_gui_textedit  READ-ONLY,
          longtext_tab TYPE catsxt_longtext_itab    READ-ONLY,
          title        TYPE sytitle                 READ-ONLY.

    METHODS: constructor
      IMPORTING
        im_title        TYPE sytitle
        im_longtext_tab TYPE catsxt_longtext_itab
        im_display_mode TYPE xfeld,

      free,

      get_text
        RETURNING
          VALUE(re_longtext) TYPE catsxt_longtext_itab,

      start.

ENDCLASS.                    "lcl_simple_text_editor DEFINITION

* Begin YEKAL0K026011
CLASS lcl_simple_text_editor IMPLEMENTATION.
  METHOD constructor.

    title        = im_title.
    longtext_tab = im_longtext_tab.
    display_mode = im_display_mode.

  ENDMETHOD.                    "constructor

  METHOD free.
    CALL METHOD: editor->free,
                 container->free.
  ENDMETHOD.                    "free

  METHOD get_text.
    DATA: lf_count TYPE sytabix.

    CALL METHOD editor->get_text_as_r3table
      IMPORTING
        table           = longtext_tab
      EXCEPTIONS
        error_dp        = 1
        error_dp_create = 2
        OTHERS          = 3.

    CALL METHOD cl_gui_cfw=>flush.

    lf_count = lines( longtext_tab ).

    DO.
      DELETE longtext_tab FROM  lf_count
                     WHERE table_line IS INITIAL.
      IF sy-subrc IS INITIAL AND lf_count > 1.
        SUBTRACT 1 FROM lf_count.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    re_longtext = longtext_tab.

  ENDMETHOD.                    "get_text

  METHOD start.
    CREATE OBJECT container
      EXPORTING
        container_name              = 'LONGTEXT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc IS INITIAL.

    CREATE OBJECT editor
      EXPORTING
        parent                 = container
        wordwrap_mode          = '2'
        wordwrap_position      = '72'
      EXCEPTIONS
        error_cntl_create      = 1
        error_cntl_init        = 2
        error_cntl_link        = 3
        error_dp_create        = 4
        gui_type_not_supported = 5
        OTHERS                 = 6.

    CHECK sy-subrc IS INITIAL.

    IF NOT display_mode IS INITIAL.
*     Set control to display only
      CALL METHOD editor->set_readonly_mode
        EXPORTING
          readonly_mode = editor->true.
    ENDIF.

    CALL METHOD editor->set_text_as_r3table
      EXPORTING
        table           = longtext_tab
      EXCEPTIONS
        error_dp        = 1
        error_dp_create = 2
        OTHERS          = 3.

    CALL METHOD editor->set_toolbar_mode
      EXPORTING
        toolbar_mode = cl_gui_textedit=>false.

    CALL METHOD editor->set_statusbar_mode
      EXPORTING
        statusbar_mode = cl_gui_textedit=>false.

  ENDMETHOD.                    "start

ENDCLASS.                    "lcl_simple_text_editor IMPLEMENTATION

****************************************************************
* VARIAVEIS globais
****************************************************************
DATA: t_zsdt0276   TYPE TABLE OF zsdt0276,
      w_zsdt0276   TYPE zsdt0276,
      w_zsdt0277   TYPE zsdt0277,
*
      g_dt_baixa   TYPE zsdt0276-dt_baixa,
      g_baixar     TYPE zsdt0276-baixar,
*
      ok_code      TYPE syst_ucomm,
      l_id_baixa   TYPE zid_baixa,
      l_chave      TYPE char26,
      l_docnum     TYPE char10          VALUE '0000000000',
      l_itmnum     TYPE char10          VALUE '000000',
      l_observacao TYPE zsdt0276-observacao,
*
      w_vrm_id     TYPE vrm_id,
      t_vrm_values TYPE vrm_values,
      w_vrm_value  TYPE vrm_value,
*
      zeditor      TYPE REF TO lcl_simple_text_editor,
      t_text       TYPE catsxt_longtext_itab,
      t_text_anx   TYPE catsxt_longtext_itab,
      w_text_anx   TYPE txline,
      t_observ     TYPE TABLE OF tline,
      w_text       TYPE textline,
      w_observ     TYPE tline,
      w_header     TYPE thead,
      l_id         TYPE thead-tdid,
      l_object     TYPE thead-tdobject,
      l_name_text  TYPE thead-tdname,
*
      l_obj_key    TYPE sibflporb-instid,
      l_loio       TYPE bds_sloio,
      l_lines      TYPE i,
      l_seq        TYPE char2,
      t_anexos     TYPE TABLE OF bdn_con,
      l_ip_mode    TYPE sgs_rwmod,
      l_ip_service TYPE sgs_srvnam,
      w_bor        TYPE borident,
      anexo_obj    TYPE REF TO cl_gos_manager.

****************************************************************
****************************************************************
