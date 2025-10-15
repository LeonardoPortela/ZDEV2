FUNCTION zsdmf_estornar_trv_cmb .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_DOC_SIMULACAO) TYPE  ZSDED003
*"     REFERENCE(IV_VBELN) TYPE  VBELN
*"     REFERENCE(IV_COMMIT) TYPE  FLAG DEFAULT SPACE
*"  EXPORTING
*"     REFERENCE(EV_ERRO) TYPE  FLAG
*"  TABLES
*"      ET_TRAVAS STRUCTURE  ZSDT0090 OPTIONAL
*"----------------------------------------------------------------------

  CHECK iv_vbeln IS NOT INITIAL.

  CLEAR et_travas[].

  SELECT * FROM zsdt0090
    INTO TABLE @DATA(lt_0090_ativo)
      WHERE doc_simulacao = @iv_doc_simulacao
        AND vbelv = @iv_vbeln
        AND categoria IN ('C','D')
        AND estorno = @space.

  CHECK sy-subrc EQ 0.

  SELECT * FROM zsdt0090
    INTO TABLE @DATA(lt_0090_rec) " recuperar
      FOR ALL ENTRIES IN @lt_0090_ativo
      WHERE doc_simulacao = @lt_0090_ativo-doc_simulacao
        AND sequencia = @lt_0090_ativo-sequenciav.

  LOOP AT lt_0090_ativo ASSIGNING FIELD-SYMBOL(<fs_0090>).

    <fs_0090>-estorno = 'X'.
    <fs_0090>-usnam_e = sy-uname.
    <fs_0090>-data_atual_e = sy-datum.
    <fs_0090>-hora_atual_e = sy-uzeit.

    APPEND <fs_0090> TO et_travas.

  ENDLOOP.

  LOOP AT lt_0090_rec ASSIGNING <fs_0090>.

    <fs_0090>-usnam = sy-uname.
    <fs_0090>-data_atual = sy-datum.
    <fs_0090>-hora_atual = sy-uzeit.

    CLEAR <fs_0090>-estorno.
    CLEAR <fs_0090>-usnam_e.
    CLEAR <fs_0090>-data_atual_e.
    CLEAR <fs_0090>-hora_atual_e.
    CLEAR <fs_0090>-vbelv_agp.

    "<fs_0090>-sequenciav = space.
    "<fs_0090>-sequenciao = space.

    PERFORM f_get_next_seq
    IN PROGRAM saplzgfsd0004
      USING <fs_0090>-doc_simulacao
   CHANGING <fs_0090>-sequencia.

    APPEND <fs_0090> TO et_travas.

  ENDLOOP.

  IF iv_commit = abap_true AND ev_erro = abap_false.

    PERFORM f_upd_seq_0090 IN PROGRAM saplzgfsd0004 CHANGING et_travas.

    MODIFY zsdt0090 FROM TABLE et_travas[].
  ENDIF.

ENDFUNCTION.
