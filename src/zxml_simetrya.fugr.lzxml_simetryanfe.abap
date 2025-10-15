tables: znfecomex.

data: nfe_alterando  like sy-ucomm,
      nfe_ex_ok_code like sy-ucomm,
      nfe_wa_fcode   type sy-ucomm,
      nfe_it_fcode   like table of nfe_wa_fcode,
      aux_znfecomex  type znfecomex.

constants: nfe_sair type c length 04 value 'SAIR',
           nfe_alt  type c length 07 value 'ALTERAR',
           nfe_con  type c length 09 value 'CONFIRMAR',
           nfe_can  type c length 08 value 'CANCELAR'.

*----------------------------------------------------------------------*
***INCLUDE LZXML_SIMETRYANFE .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9980  OUTPUT
*&---------------------------------------------------------------------*
module status_9980 output.

  clear: nfe_it_fcode[].

  if nfe_alterando is not initial.
    nfe_wa_fcode = nfe_sair.
    append nfe_wa_fcode to nfe_it_fcode.
    nfe_wa_fcode = nfe_alt.
    append nfe_wa_fcode to nfe_it_fcode.
  else.
    nfe_wa_fcode = nfe_con.
    append nfe_wa_fcode to nfe_it_fcode.
    nfe_wa_fcode = nfe_can.
    append nfe_wa_fcode to nfe_it_fcode.
  endif.

  "nfe_alterando
  loop at screen.
    if ( screen-group1 eq 'ALT' ) and ( nfe_alterando is not initial ).
      screen-output = '1'.
      screen-input  = '1'.
    else.
      screen-output = '1'.
      screen-input  = '0'.
    endif.
    modify screen.
  endloop.

  set pf-status 'PFNFEEXP' excluding nfe_it_fcode.
  set titlebar 'TLNFEEXP'.

endmodule.                 " STATUS_9980  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  NFE_EXPORTACAO  INPUT
*&---------------------------------------------------------------------*
module nfe_exportacao input.

  case nfe_ex_ok_code.
    when nfe_sair.
      leave to screen 0.
  endcase.

endmodule.                 " NFE_EXPORTACAO  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9980  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9980 input.

  data: valida_uf_local type c length 1.

  case nfe_ex_ok_code.
    when nfe_alt.
      nfe_alterando = c_x.
      move znfecomex to aux_znfecomex.
    when nfe_con.
      perform verificar_insert_uf_local using znfecomex valida_uf_local.
      if valida_uf_local eq c_x.
        modify znfecomex.
        clear: nfe_alterando.
      endif.
    when nfe_can.
      clear: nfe_alterando.
      move aux_znfecomex to znfecomex.
  endcase.

endmodule.                 " USER_COMMAND_9980  INPUT
*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_INSERT_UF_LOCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZNFECOMEX  text
*      -->P_VALIDA_UF  text
*----------------------------------------------------------------------*
form verificar_insert_uf_local  using    p_znfecomex
                                         p_valida_uf_local.


  clear: p_valida_uf_local.

  if znfecomex-ufembarq is initial.
    message i023 with 'Deve ser informado o estado UF'.
    exit.
  endif.

  if  znfecomex-xlocembarq is initial.
    message i023 with 'Deve ser informado o local de embarque!'.
    exit.
  endif.

  p_valida_uf_local = 'X'.

endform.                    " VERIFICAR_INSERT_UF_LOCAL

*&---------------------------------------------------------------------*
*&      Form  VISIBILIDADE_NFE_EXP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
