FUNCTION zsd_valida_nf_formacao_lote.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM
*"     REFERENCE(I_DIRECAO) TYPE  C
*"  EXPORTING
*"     REFERENCE(E_NFS_VINCULADAS) TYPE  C
*"----------------------------------------------------------------------
  CLEAR: e_nfs_vinculadas.

  IF i_direcao EQ 'E'.

    SELECT docnum_eprod
      FROM zsdtvinc_p_flote
      INTO @DATA(lv_docnum)
      UP TO 1 ROWS
      WHERE docnum_eprod EQ @i_docnum
        AND cancel       EQ @abap_false.
    ENDSELECT.
    IF sy-subrc IS INITIAL.
      e_nfs_vinculadas = abap_true.
    ENDIF.

  ELSEIF i_direcao EQ 'S'.

    SELECT docnum_flote
     FROM zsdtvinc_p_flote
     INTO lv_docnum
     UP TO 1 ROWS
     WHERE docnum_flote EQ i_docnum
       AND cancel       EQ abap_false.
    ENDSELECT.
    IF sy-subrc IS INITIAL.
      e_nfs_vinculadas = abap_true.
    ENDIF.

  ENDIF.

ENDFUNCTION.
