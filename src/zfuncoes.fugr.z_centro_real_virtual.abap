function Z_CENTRO_REAL_VIRTUAL.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(BUKRS) TYPE  BUKRS OPTIONAL
*"     REFERENCE(CENTRO) TYPE  WERKS_D
*"     REFERENCE(TP_CENTRO_OUT) TYPE  CHAR01 DEFAULT 'R'
*"  EXPORTING
*"     REFERENCE(CENTRO_REAL) TYPE  WERKS_D
*"     REFERENCE(CENTRO_VIRTUAL) TYPE  WERKS_D
*"     REFERENCE(TP_CENTRO_IN) TYPE  CHAR01
*"     REFERENCE(WA_J_1BBRANCH) TYPE  J_1BBRANCH
*"     VALUE(CENTRO_OUT) TYPE  WERKS_D
*"  EXCEPTIONS
*"      INFORMAR_CENTRO
*"      NAO_CENTRO_R_VIRTUAL
*"      INFORMAR_CENTRO_OUT
*"      INFORMAR_CENTRO_V
*"--------------------------------------------------------------------

  data: wa_centro_real type zsdt_depara_cen.

  if centro is initial.
    message e000 raising informar_centro.
  endif.

  if ( tp_centro_out ne c_v ) and ( tp_centro_out ne c_r ) .
    message e002 raising informar_centro_out.
  endif.

  clear: tp_centro_in, centro_real, centro_virtual, wa_j_1bbranch.

  "Verifica se é centro real
  if bukrs is initial.
    select single * into wa_j_1bbranch
      from j_1bbranch
     where branch eq centro.
  else.
    select single * into wa_j_1bbranch
      from j_1bbranch
     where bukrs  eq bukrs
       and branch eq centro.
  endif.

  "Achou centro real
  if sy-subrc is initial.
    tp_centro_in = c_r.
    centro_real  = wa_j_1bbranch-branch.
    bukrs        = wa_j_1bbranch-bukrs.

    if tp_centro_out eq c_v.
      message e003 raising informar_centro_v.
    endif.

  else.

    if bukrs is initial.
      select single * into wa_centro_real
        from zsdt_depara_cen
       where centrov_1 eq centro.
    else.
      select single * into wa_centro_real
        from zsdt_depara_cen
       where vkorg     eq bukrs
         and centrov_1 eq centro.
    endif.

    if sy-subrc is initial.
      tp_centro_in   = c_v.
      centro_real    = wa_centro_real-centro_real.
      centro_virtual = wa_centro_real-centrov_1.
      bukrs          = wa_centro_real-vkorg.

      select single * into wa_j_1bbranch
        from j_1bbranch
       where branch eq centro_real.

    else.
      message e001 with centro raising nao_centro_r_virtual.
    endif.

  endif.

  case tp_centro_out.
    when c_r.
      centro_out = centro_real.
    when c_v.
      centro_out = centro.
  endcase.

endfunction.
