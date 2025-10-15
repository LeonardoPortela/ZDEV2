FUNCTION zsdmf_criar_trv_cmb_com_ref .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IR_DOC_SIMULACAO) TYPE  ZSD_RANGE_DOCSI_T
*"     REFERENCE(IR_VBELN_DE) TYPE  SHP_VBELN_RANGE_T
*"     REFERENCE(IV_VBELN_PARA) TYPE  VBELN
*"     REFERENCE(IV_COMMIT) TYPE  FLAG DEFAULT SPACE
*"     REFERENCE(IV_COM_AGRP) TYPE  FLAG DEFAULT SPACE
*"  EXPORTING
*"     REFERENCE(EV_ERRO) TYPE  FLAG
*"  TABLES
*"      ET_TRAVAS STRUCTURE  ZSDT0090 OPTIONAL
*"----------------------------------------------------------------------

  CHECK ir_vbeln_de[] IS NOT INITIAL.
  CHECK iv_vbeln_para IS NOT INITIAL.

  CLEAR et_travas[].

  SELECT * FROM zsdt0090
    INTO TABLE @DATA(lt_0090_de)
      WHERE doc_simulacao IN @ir_doc_simulacao
        AND vbelv IN @ir_vbeln_de
        AND categoria IN ('C','D')
        AND estorno = @space.

  DATA(lt_0090_para) = lt_0090_de.

  LOOP AT lt_0090_de ASSIGNING FIELD-SYMBOL(<fs_0090>).

    IF iv_com_agrp = abap_true.
      <fs_0090>-vbelv_agp = iv_vbeln_para.
    ENDIF.

    <fs_0090>-estorno = 'X'.
    <fs_0090>-usnam_e = sy-uname.
    <fs_0090>-data_atual_e = sy-datum.
    <fs_0090>-hora_atual_e = sy-uzeit.

    APPEND <fs_0090> TO et_travas.

  ENDLOOP.

  LOOP AT lt_0090_para ASSIGNING <fs_0090>.

    IF iv_com_agrp = abap_true.
      <fs_0090>-vbelv_agp = iv_vbeln_para.
    ENDIF.

    <fs_0090>-usnam = sy-uname.
    <fs_0090>-data_atual = sy-datum.
    <fs_0090>-hora_atual = sy-uzeit.

    <fs_0090>-vbelv = iv_vbeln_para.

    <fs_0090>-sequenciav = <fs_0090>-sequencia.
    <fs_0090>-sequenciao = space.

    PERFORM f_get_next_seq
    IN PROGRAM saplzgfsd0004
      USING <fs_0090>-doc_simulacao
   CHANGING <fs_0090>-sequencia.

    APPEND <fs_0090> TO et_travas.

  ENDLOOP.

  IF iv_commit = abap_true AND ev_erro = abap_false.
    MODIFY zsdt0090 FROM TABLE et_travas[].
  ENDIF.

ENDFUNCTION.
