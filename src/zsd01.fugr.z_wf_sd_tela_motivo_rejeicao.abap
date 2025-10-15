FUNCTION Z_WF_SD_TELA_MOTIVO_REJEICAO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(VBELN) TYPE  VBELN
*"  TABLES
*"      TEXTO_CONTAINER STRUCTURE  ZWF_TEXTO_REJEICAO
*"----------------------------------------------------------------------
*DATA: CONTAINER TYPE REF TO cl_gui_custom_container,
*      obj_texto TYPE REF TO c_textedit_control.

*limpa tabelas internas
  CLEAR:   texto_container, tg_texttable.
  REFRESH: texto_container, tg_texttable.

* Inicializa container
  IF container IS INITIAL.
    CREATE OBJECT container
      EXPORTING
        container_name = 'CONTAINER'.
  ENDIF.

* limpa conteúdo do objeto
  IF NOT obj_texto IS INITIAL.
    CALL METHOD obj_texto->free.
  ENDIF.

* Cria o objeto
  CREATE OBJECT obj_texto
    EXPORTING
*    max_number_chars       =
*    style                  = 0
      wordwrap_mode          = cl_gui_textedit=>wordwrap_at_fixed_position
      wordwrap_position      = 72
      wordwrap_to_linebreak_mode = cl_gui_textedit=>false
*    filedrop_mode          = dropfile_event_off
      parent                 = container
*    lifetime               =
*    name                   =
EXCEPTIONS
  error_cntl_create      = 1
  error_cntl_init        = 2
  error_cntl_link        = 3
  error_dp_create        = 4
  gui_type_not_supported = 5
  OTHERS                 = 6.

* Exportar parâmetro.
  EXPORT VBELN TO MEMORY ID 'ZSDWFP1'.

* Chama a tela onde será inserido o motivo da recusa
  CALL SCREEN '9000'.

  texto_container[] = tg_texttable[].

*limpa tabelas internas
  CLEAR tg_texttable.
  REFRESH tg_texttable.



ENDFUNCTION.
