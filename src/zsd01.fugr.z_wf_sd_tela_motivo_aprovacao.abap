FUNCTION Z_WF_SD_TELA_MOTIVO_APROVACAO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(VBELN) TYPE  VBELN
*"  TABLES
*"      TEXTO_CONTAINER_AP STRUCTURE  ZWF_TEXTO_REJEICAO
*"----------------------------------------------------------------------
*DATA: CONTAINER TYPE REF TO cl_gui_custom_container,
*      obj_texto TYPE REF TO c_textedit_control.

*limpa tabelas internas
  CLEAR:   texto_container_ap, tg_texttable2.
  REFRESH: texto_container_ap, tg_texttable2.

* Inicializa container
  IF container2 IS INITIAL.
    CREATE OBJECT container2
      EXPORTING
        container_name = 'CONTAINER2'.
  ENDIF.

* limpa conteúdo do objeto
  IF NOT obj_texto2 IS INITIAL.
    CALL METHOD obj_texto2->free.
  ENDIF.

* Cria o objeto
  CREATE OBJECT obj_texto2
    EXPORTING
*    max_number_chars       =
*    style                  = 0
      wordwrap_mode          = cl_gui_textedit=>wordwrap_at_fixed_position
      wordwrap_position      = 72
      wordwrap_to_linebreak_mode = cl_gui_textedit=>false
*    filedrop_mode          = dropfile_event_off
      parent                 = container2
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
  EXPORT VBELN TO MEMORY ID 'ZSDWFAP1'.

* Chama a tela onde será inserido o motivo da aprovação
  CALL SCREEN '9001'.

  texto_container_ap[] = tg_texttable2[].

*limpa tabelas internas
  CLEAR tg_texttable2.
  REFRESH tg_texttable2.



ENDFUNCTION.
