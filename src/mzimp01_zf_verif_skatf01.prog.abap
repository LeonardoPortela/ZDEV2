*----------------------------------------------------------------------*
***INCLUDE MZIMP01_ZF_VERIF_SKATF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_VERIF_SKAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_verif_skat  USING    p_conta TYPE skat-saknr
                             p_texto TYPE skat-txt50.

  SELECT SINGLE txt50
    INTO p_texto
    FROM skat
    WHERE spras = sy-langu AND
          ktopl = v_ktopl  AND
          saknr = p_conta.

  CHECK sy-subrc <> 0.

  MESSAGE 'Conta inexistente!' TYPE c_e.

ENDFORM.                    " ZF_VERIF_SKAT
