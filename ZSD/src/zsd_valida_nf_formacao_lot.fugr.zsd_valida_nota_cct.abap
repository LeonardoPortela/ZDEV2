FUNCTION zsd_valida_nota_cct.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM
*"  EXPORTING
*"     REFERENCE(E_ENTRADA_CCT) TYPE  C
*"     REFERENCE(E_SAIDA_CCT) TYPE  C
*"     REFERENCE(E_AMBAS_CCT) TYPE  C
*"     REFERENCE(E_ZLEST0146) TYPE  ZLEST0146
*"----------------------------------------------------------------------

  DATA: wl_zlest0146_e TYPE zlest0146,
        wl_zlest0146_s TYPE zlest0146,
        lt_zlest0147 TYPE zlest0147_t,
        lt_zlest0168 TYPE zlest0168_t,
        v_doc_rateio TYPE char01.

 CLEAR: wl_zlest0146_e,
        wl_zlest0146_s.

  SELECT *
    FROM zsdtvinc_p_flote
    INTO TABLE @DATA(lt_vinc)
    WHERE docnum_eprod EQ @i_docnum
      AND cancel       EQ @abap_false.

  CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
    EXPORTING
      i_docnum            = i_docnum
*     i_itmnum            = itmnum
      i_set_peso_disp_uso = abap_true
    IMPORTING
      e_zlest0146         = wl_zlest0146_e
      e_zlest0147         = lt_zlest0147
      e_zlest0168         = lt_zlest0168
      e_doc_rateio        = v_doc_rateio.

  IF v_doc_rateio IS INITIAL AND wl_zlest0146_e IS NOT INITIAL.
    e_entrada_cct = abap_true.
    e_zlest0146 = wl_zlest0146_e.
  ENDIF.

  IF lt_vinc IS NOT INITIAL.

    LOOP AT lt_vinc ASSIGNING FIELD-SYMBOL(<fs_vinc>).

      FREE: lt_zlest0147,
            lt_zlest0168.

      CLEAR: v_doc_rateio.

      CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
        EXPORTING
          i_docnum            = <fs_vinc>-docnum_flote
*         i_itmnum            = itmnum
          i_set_peso_disp_uso = abap_false
        IMPORTING
          e_zlest0146         = wl_zlest0146_s
          e_zlest0147         = lt_zlest0147
          e_zlest0168         = lt_zlest0168
          e_doc_rateio        = v_doc_rateio.

      IF v_doc_rateio IS INITIAL AND wl_zlest0146_s IS NOT INITIAL.

        e_saida_cct = abap_true.
        e_zlest0146 = wl_zlest0146_s.
        EXIT.

      ELSE.

        CLEAR: e_saida_cct.
        EXIT.

      ENDIF.

    ENDLOOP.

  ENDIF.

  IF e_entrada_cct IS NOT INITIAL AND
     e_saida_cct IS NOT INITIAL.

    e_ambas_cct = abap_true.

    CLEAR:e_entrada_cct,
          e_saida_cct,
          e_zlest0146.

  ENDIF.

ENDFUNCTION.
