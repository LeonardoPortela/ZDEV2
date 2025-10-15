*----------------------------------------------------------------------*
***INCLUDE LZGSD_BAIXA_COMPRAF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_GERAR NUMERO
*&---------------------------------------------------------------------*
FORM f_gerar_id_baixa.

  CHECK l_id_baixa IS INITIAL.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'Z_ID_BAIXA'
    IMPORTING
      number                  = l_id_baixa
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF sy-subrc <> 0.
    RAISE id_baixa_not_found.
  ENDIF.

  CONCATENATE l_id_baixa l_docnum l_itmnum
         INTO l_chave.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SALVAR_BAIXA
*&---------------------------------------------------------------------*
FORM f_salvar_baixa .

  FREE: zsdt0277.

*-------------------------------------------
* gerar ID
*-------------------------------------------
  PERFORM f_gerar_id_baixa.

*-------------------------------------------
* gravar tabelas
*-------------------------------------------
  LOOP AT t_zsdt0276 INTO w_zsdt0276.

    FREE: t_observ,
          l_observacao,
          t_text,
          zsdt0276.

*--------------------------------------------
*---recuperar texto
*--------------------------------------------
    t_text = zeditor->get_text( ).

    LOOP AT t_text   INTO w_text.
      IF sy-tabix = 1.
        l_observacao = w_text.
      ELSE.
        CONCATENATE l_observacao w_text
               INTO l_observacao
          SEPARATED BY space.
      ENDIF.
      w_observ-tdformat = '*'.
      w_observ-tdline   = w_text.
      APPEND w_observ  TO t_observ.
    ENDLOOP.

*--------------------------------------------
*---gravar tabela
*--------------------------------------------
    zsdt0276-mandt      = sy-mandt.
    zsdt0276-id_baixa   = l_id_baixa.
    zsdt0276-docnum     = w_zsdt0276-docnum.
    zsdt0276-itmnum     = w_zsdt0276-itmnum.
    zsdt0276-menge      = w_zsdt0276-menge.
    zsdt0276-valor      = w_zsdt0276-valor.
    zsdt0276-dt_baixa   = g_dt_baixa.
    zsdt0276-baixar     = COND #( WHEN g_baixar = 'S' THEN 'X'
                                  WHEN g_baixar = 'N' THEN ' ' ).
    zsdt0276-observacao = l_observacao.
    MODIFY zsdt0276.

*--------------------------------------------
*---gravar texto
*--------------------------------------------
    l_name_text       = l_chave.
    w_header-tdobject = 'ZOBSERVAC2'.
    w_header-tdname   = l_name_text.
    w_header-tdid     = 'BXNF'.
    w_header-tdspras  = sy-langu.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = sy-mandt
        header          = w_header
*       INSERT          = ' '
        savemode_direct = 'X'
      TABLES
        lines           = t_observ
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.

  ENDLOOP.

*--------------------------------------------
*---gravar zsdt0277
*--------------------------------------------
  zsdt0277-mandt      = sy-mandt.
  zsdt0277-id_baixa   = l_id_baixa.
  zsdt0277-data       = sy-datum.
  zsdt0277-hora       = sy-uzeit.
  zsdt0277-usuario    = sy-uname.
  MODIFY zsdt0277.

  COMMIT WORK AND WAIT.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ANEXA_DOCOS
*&---------------------------------------------------------------------*
FORM f_anexa_doctos .

  FREE: t_anexos.

*-------------------------------------------
* gerar ID
*-------------------------------------------
  PERFORM f_gerar_id_baixa.

  l_obj_key = l_chave.

  CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
    EXPORTING
      classname          = 'ZS_BAIXANF'
      objkey             = l_obj_key
      client             = sy-mandt
    TABLES
      gos_connections    = t_anexos
    EXCEPTIONS
      no_objects_found   = 1
      internal_error     = 2
      internal_gos_error = 3
      OTHERS             = 4.

  DESCRIBE TABLE t_anexos LINES l_lines.

  CREATE OBJECT anexo_obj TYPE cl_gos_manager.

  l_ip_mode     = 'E'.
  l_ip_service  = COND #( WHEN l_lines = 0 THEN 'PCATTA_CREA'
                                           ELSE 'VIEW_ATTA' ).
  w_bor-objkey  = l_chave.
  w_bor-objtype = 'ZS_BAIXANF'.

  anexo_obj->set_rw_mode( ip_mode = l_ip_mode ).

  anexo_obj->start_service_direct(
    EXPORTING
      ip_service         = l_ip_service
      is_object          = w_bor
    EXCEPTIONS
      no_object          = 1
      object_invalid     = 2
      execution_failed   = 3
      OTHERS             = 4 ).

  WAIT UP TO 2 SECONDS.

  COMMIT WORK.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_exibe anexos
*&---------------------------------------------------------------------*
FORM f_exibe_doctos.

  FREE: t_anexos,
        t_text_anx.

  l_obj_key = l_chave.

  CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
    EXPORTING
      classname          = 'ZS_BAIXANF'
      objkey             = l_obj_key
      client             = sy-mandt
    TABLES
      gos_connections    = t_anexos
    EXCEPTIONS
      no_objects_found   = 1
      internal_error     = 2
      internal_gos_error = 3
      OTHERS             = 4.

  CHECK t_anexos[] IS NOT INITIAL.

  LOOP AT t_anexos  INTO DATA(w_anexos).
    l_seq = sy-tabix.
    CONCATENATE l_seq '. ' w_anexos-descript '.' w_anexos-docuclass
           INTO w_text_anx.
    APPEND w_text_anx TO t_text_anx.
  ENDLOOP.

  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      im_title        = text-030
      im_display_mode = abap_true
      im_no_toolbar   = abap_true
      im_start_column = 110
      im_start_row    = 09
    CHANGING
      ch_text         = t_text_anx.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_elimina
*&---------------------------------------------------------------------*
FORM f_elimina_anexos.

  FREE: t_anexos, l_loio.

  l_obj_key = l_chave.

  CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
    EXPORTING
      classname          = 'ZS_BAIXANF'
      objkey             = l_obj_key
      client             = sy-mandt
    TABLES
      gos_connections    = t_anexos
    EXCEPTIONS
      no_objects_found   = 1
      internal_error     = 2
      internal_gos_error = 3
      OTHERS             = 4.

  CHECK t_anexos[] IS NOT INITIAL.

  LOOP AT t_anexos INTO DATA(w_anexos).
    DELETE FROM srgbtbrel WHERE reltype  = 'ATTA'
                            AND instid_a = l_chave
                            AND typeid_a = 'ZS_BAIXANF'
                            AND catid_a  = 'BO'
                            AND instid_b = w_anexos-loio_id.

*    l_loio-loio_id = w_anexos-loio_id.
*    CALL FUNCTION 'BDS_CONNECTION_DELETE'
*      EXPORTING
*        classname       = 'ZS_BAIXANF'
*        classtype       = w_anexos-classtype
*        object_key      = l_obj_key
*        loio            = l_loio
*      EXCEPTIONS
*        nothing_found   = 1
*        parameter_error = 2
*        not_allowed     = 3
*        error_kpro      = 4
*        internal_error  = 5
*        not_authorized  = 6
*        OTHERS          = 9.

  ENDLOOP.

  COMMIT WORK.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
