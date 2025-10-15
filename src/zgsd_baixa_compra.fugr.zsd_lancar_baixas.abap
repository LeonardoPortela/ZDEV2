FUNCTION zsd_lancar_baixas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ID_BAIXA) TYPE  ZID_BAIXA OPTIONAL
*"  TABLES
*"      T_DOCUMENTOS STRUCTURE  ZSDT0276 OPTIONAL
*"  EXCEPTIONS
*"      ID_BAIXA_NOT_FOUND
*"----------------------------------------------------------------------

  FREE: t_zsdt0276,
        w_zsdt0276,
        l_id_baixa,
        l_chave,
        t_observ,
        t_text,
        ok_code.

*-------------------------------
* seleciona tabela ou criar novo id
*-------------------------------
  IF i_id_baixa IS NOT INITIAL.
    SELECT *
      FROM zsdt0276
      INTO TABLE t_documentos
     WHERE id_baixa = i_id_baixa.

    READ TABLE t_documentos INTO w_zsdt0276 INDEX 1.

    l_id_baixa = i_id_baixa.

    CONCATENATE l_id_baixa l_docnum l_itmnum
           INTO l_chave.
  ENDIF.

*-------------------------------
* set variaveis
*-------------------------------
  t_zsdt0276[] = t_documentos[].

  g_dt_baixa   = COND #( WHEN w_zsdt0276        IS INITIAL THEN sy-datum
                                                           ELSE w_zsdt0276-dt_baixa ).
  g_baixar     = COND #( WHEN l_id_baixa        IS INITIAL THEN ' '
                         WHEN w_zsdt0276-baixar IS INITIAL THEN 'N'
                                                           ELSE 'S' ).

*-------------------------------
* verifica se tem texto ao ID
*-------------------------------
  l_id         = 'BXNF'.
  l_object     = 'ZOBSERVAC2'.
  l_name_text  = l_chave.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = l_id
      language                = sy-langu
      name                    = l_name_text
      object                  = l_object
    TABLES
      lines                   = t_observ
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  LOOP AT t_observ INTO w_observ.
    w_text = w_observ-tdline.
    APPEND w_text    TO t_text.
  ENDLOOP.

*-------------------------------
* pop-up
*-------------------------------
  CALL SCREEN 0200 STARTING AT  60 03
                     ENDING AT 120 18.

ENDFUNCTION.
