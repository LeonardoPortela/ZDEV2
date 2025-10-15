*----------------------------------------------------------------------*
***INCLUDE ZXF05F02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CONSISTIR_CNPJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_LFA1_STCD1  text
*----------------------------------------------------------------------*
form f_consistir_cnpj  using p_but000 type but000
                             p_lifnr p_stcd1 p_stcd3 p_regio p_ktokk. "*-CS2024000622-19.09.2024-JT-#152691-fim

  data: v_lifnr    type lfa1-lifnr,
        v_stcd1    type lfa1-stcd1,
        vl_sperr   type lfa1-sperr,
        v_text1    type spop-textline2,
        v_text2    type spop-textline2,
        v_res      type c length 1,
        v_res1     type c length 1,
        v_pergunta type c length 1,
        vstate     type setleaf-valfrom.

  check not p_stcd1 is initial.
  v_res1 = 'S'.

  select single valfrom
    from setleaf
    into vstate
   where setname = 'MAGGI_FORN_DUPL'
     and valfrom = p_regio .

  if sy-subrc = 0.
    v_pergunta = abap_true.
  endif.

*-CS2024000622-19.09.2024-JT-#152691-incio
*  SELECT lifnr    stcd1    sperr
*    INTO (v_lifnr, v_stcd1,vl_sperr)
*    FROM lfa1
*   UP TO 1 ROWS
*   WHERE stcd1  = p_stcd1
*     AND lifnr <> p_lifnr
*     AND sperr NE 'X'.
*  ENDSELECT.

  select dfkkbptaxnum~*
    from dfkkbptaxnum
   inner join ibpsupplier   on ibpsupplier~businesspartner = dfkkbptaxnum~partner
   inner join lfa1          on lfa1~lifnr                  = ibpsupplier~supplier
    into table @data(t_tax)
   where taxnum     = @p_stcd1
     and taxtype    = 'BR1'
     and ktokk      = @p_ktokk     "*-CS2024000622-19.09.2024-JT-#152691-fim
     and lfa1~sperr = @abap_false
     and lfa1~loevm = @abap_false
     and lfa1~nodel = @abap_false.

  data(t_fornece) = t_tax[].

  delete t_fornece where partner = p_but000-partner. "p_lifnr.

  read table t_fornece into data(w_fornece) index 1.

  check sy-subrc = 0.

  v_lifnr  = w_fornece-partner.  "*-CS2024000622-19.09.2024-JT-#152691-fim
  v_stcd1  = w_fornece-taxnum.   "*-CS2024000622-19.09.2024-JT-#152691-fim
*-CS2024000622-19.09.2024-JT-#152691-incio

  case v_pergunta.
    when abap_true.
* --->> cs1014779 - IR106423 ---->>
      concatenate 'Código:' v_lifnr '-' v_stcd1 into v_text1.
*     CONCATENATE 'CNPJ:' v_stcd1 INTO v_text1.
      v_text2 = 'CNPJ DUPLICADO! Deseja criar um novo Parceiro de Negócio?'.

      if sy-batch = abap_off.
        call function 'POPUP_TO_CONFIRM_STEP'
          exporting
            defaultoption = 'N'
            titel         = 'ATENÇÃO'
            textline1     = v_text1
            textline2     = v_text2
          importing
            answer        = v_res.
      else.
        v_res = 'N'.
      endif.

      if v_res eq 'N' or v_res eq 'A'.
        message e008(zfi) raising error. "CNPJ Duplicado
      else.
        perform f_consistir_cnpj_insc using p_but000
                                            p_lifnr
                                            p_stcd1
                                            p_stcd3
                                            abap_false.
      endif.

    when abap_false.
      message e008(zfi) raising error. "CNPJ Duplicado
  endcase.

*  IF v_res1 = 'N'.
**<< --- cs1014779 - IR106423 <----
*    MESSAGE e008(zfi) RAISING error. "CNPJ Duplicado
*  ENDIF.

endform.                    " F_CONSISTIR_CNPJ

form f_consistir_cuit  using   p_but000 type but000
                               p_lifnr p_stcd1.

  data: v_lifnr  type lfa1-lifnr,
        v_stcd1  type lfa1-stcd1,
        vl_sperr type lfa1-sperr.

  check not p_stcd1 is initial.

*-CS2024000622-19.09.2024-JT-#152691-incio
*  SELECT lifnr stcd1 sperr INTO (v_lifnr, v_stcd1,vl_sperr)
*     FROM lfa1 UP TO 1 ROWS
*     WHERE stcd1 = p_stcd1
*       AND lifnr <> p_lifnr
*       AND sperr NE 'X'.
*  ENDSELECT.

  select dfkkbptaxnum~*
    from dfkkbptaxnum
   inner join ibpsupplier   on ibpsupplier~businesspartner = dfkkbptaxnum~partner
   inner join lfa1          on lfa1~lifnr                  = ibpsupplier~supplier
    into table @data(t_tax)
   where taxnum     = @p_stcd1
     and taxtype    = 'AR1A' "// WBARBOSA 22/09/2025
     and lfa1~sperr = @abap_false
     and lfa1~loevm = @abap_false
     and lfa1~nodel = @abap_false.

  data(t_fornece) = t_tax[].

  delete t_fornece where partner = p_but000-partner. "p_lifnr.

  read table t_fornece into data(w_fornece) index 1.

  check sy-subrc = 0.

  v_lifnr = w_fornece-partner.  "*-CS2024000622-19.09.2024-JT-#152691-fim
*-CS2024000622-19.09.2024-JT-#152691-fim

  if sy-subrc = 0.
    message e057(zfi) with v_lifnr raising error. "CUIT Duplicado
  endif.
endform.                    " F_CONSISTIR_CNPJ
