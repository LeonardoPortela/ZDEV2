*&---------------------------------------------------------------------*
*&  Include           ZIM09_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  RECUPERA_VALORES_112
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM recupera_valores_112 .

  ASSIGN ('(ZIM09)s_bukrs2[]') TO <r_bukrs2>.
  ASSIGN ('(ZIM09)p_gjahr2')   TO <r_gjahr2>.
  ASSIGN ('(ZIM09)p_safra2') TO <r_safra2>.

ENDFORM.                    " RECUPERA_VALORES_112
*&---------------------------------------------------------------------*
*&      Form  RECUPERA_VALORES_111
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM recupera_valores_111 .

  ASSIGN ('(ZIM09)s_bukrs[]') TO <r_bukrs>.
  ASSIGN ('(ZIM09)p_datai')   TO <r_datai>.
  ASSIGN ('(ZIM09)p_dataf')   TO <r_dataf>.
  ASSIGN ('(ZIM09)p_dtini')   TO <r_dtini>.


ENDFORM.                    " RECUPERA_VALORES_111
*&---------------------------------------------------------------------*
*&      Form  EXECUTA_COPIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM executa_copia .

  DATA: BEGIN OF t_origem OCCURS 0.
          INCLUDE STRUCTURE zim02_sol_ap_ctl .
  DATA: END OF t_origem.

  DATA: BEGIN OF t_destino OCCURS 0.
          INCLUDE STRUCTURE zim02_sol_ap_ctl .
  DATA: END OF t_destino.
  DATA: w_safra2 TYPE zim02_sol_ap_ctl-safra2.

  CLEAR: t_origem, t_destino, w_safra2.

  IF NOT modelo_safra IS INITIAL.
    w_safra2 = modelo_safra + 1.
  ENDIF.


  SELECT * FROM zim02_sol_ap_ctl INTO TABLE t_origem
    WHERE bukrs  IN <r_bukrs>    AND
          ano    EQ modelo_exerc AND
          safra  EQ modelo_safra AND
          safra2 EQ w_safra2.

  IF sy-subrc <> 0.
    MESSAGE i000(z01) WITH 'Nenhum registro encontrado para seleção.'.
  ELSE.

    LOOP AT t_origem.
      MOVE-CORRESPONDING t_origem TO t_destino.

      t_destino-bukrs = t_origem-bukrs.
      t_destino-ano = novo_exerc.
      t_destino-safra = novo_safra.
      IF NOT novo_safra IS INITIAL.
        t_destino-safra2 = novo_safra + 1.
      ENDIF.
      t_destino-kostl = t_origem-kostl.
      t_destino-dt_aprov_in = <r_datai>.
      t_destino-dt_aprov_fim = <r_dataf>.
      t_destino-aprovador = t_origem-aprovador.
      t_destino-fase = '01'.
      t_destino-dt_inicio = <r_dtini>.

      CLEAR: t_destino-tx_usd,
             t_destino-tx_eur.

      APPEND t_destino.
      CLEAR t_destino.
    ENDLOOP.

    IF NOT t_destino[] IS INITIAL.
      MODIFY zim02_sol_ap_ctl FROM TABLE t_destino.
      COMMIT WORK AND WAIT.
      MESSAGE i000(z01) WITH 'Registro(s) atualizado(s).'.
    ENDIF.
  ENDIF.
ENDFORM.                    " EXECUTA_COPIA
*&---------------------------------------------------------------------*
*&      Form  EXECUTA_BLOQUEIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM executa_bloqueio .

  DATA: w_safra2 TYPE zim02_sol_ap_ctl-safra2.

  IF NOT <r_safra2> IS INITIAL.
    w_safra2 = <r_safra2> + 1.
  ELSE.
    CLEAR w_safra2.
  ENDIF.

  UPDATE zim02_sol_ap_ctl
  SET blq_exerc = 'X'
  WHERE bukrs  IN <r_bukrs2> AND
        ano    EQ <r_gjahr2> AND
        safra  EQ <r_safra2> AND
        safra2 EQ w_safra2.

  IF sy-subrc = 0.
    MESSAGE i000(z01) WITH 'Registro(s) bloqueado(s).'.
    COMMIT WORK AND WAIT.
  ELSE.
    MESSAGE i000(z01) WITH 'Nenhum registro encontrado para seleção.'.
  ENDIF.

ENDFORM.                    " EXECUTA_BLOQUEIO
*&---------------------------------------------------------------------*
*&      Form  RECUPERA_VALORES_113
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM recupera_valores_113 .

  ASSIGN ('(ZIM09)s_bukrs3[]') TO <r_bukrs3>.
  ASSIGN ('(ZIM09)p_gjahr3')   TO <r_gjahr3>.
  ASSIGN ('(ZIM09)p_safra3')   TO <r_safra3>.
  ASSIGN ('(ZIM09)p_fase3')    TO <r_fase3>.
  ASSIGN ('(ZIM09)p_datai3')   TO <r_datai3>.
  ASSIGN ('(ZIM09)p_dataf3')   TO <r_dataf3>.
  ASSIGN ('(ZIM09)p_dtini3')   TO <r_dtini3>.

ENDFORM.                    " RECUPERA_VALORES_113
