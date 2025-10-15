function Z_VERIFICA_CLI_FOR_CTA_MAT.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_KOART) TYPE  KOART
*"     VALUE(P_EMPRESA) TYPE  BUKRS OPTIONAL
*"     REFERENCE(P_FILIAL) TYPE  J_1BBRANC_ OPTIONAL
*"     REFERENCE(P_FORNECEDOR) TYPE  LIFNR OPTIONAL
*"     REFERENCE(P_CLIENTE) TYPE  KUNNR OPTIONAL
*"     REFERENCE(P_PL_CONTAS) TYPE  KTOPL OPTIONAL
*"     REFERENCE(P_CT_RAZAO) TYPE  SAKNR OPTIONAL
*"     REFERENCE(P_MATERIAL) TYPE  MATNR OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"      BRANCH
*"--------------------------------------------------------------------

  data: wa_kna1 type kna1,
        wa_knb1 type knb1,
        wa_lfa1 type lfa1,
        wa_lfb1 type lfb1,
        wa_skb1 type skb1,
        wa_j_1bbranch type j_1bbranch,
        wa_virtual    type zsdt_depara_cen,
        p_filial_r    type j_1bbranc_.

  if ( p_empresa is initial ) and ( p_filial is not initial ).

    select single * into wa_virtual
      from zsdt_depara_cen
     where centrov_1 eq p_filial.

    if sy-subrc is initial.
      p_filial_r = wa_virtual-centro_real.
    else.
      p_filial_r = p_filial.
    endif.

    select single * into wa_j_1bbranch
      from j_1bbranch
     where branch eq p_filial_r.
    if sy-subrc is initial.
      p_empresa = wa_j_1bbranch-bukrs.
    else.
      message e011(z01) with 'Empresa não encontrada p/ Local de Negócio' p_filial raising branch.
    endif.
  endif.

  case p_koart.
    when 'A'.
*A  Anexos
    when 'D'.
*D  Cliente
      if not p_cliente is initial.
        select single * into wa_kna1 from kna1 where kunnr eq p_cliente.
        if sy-subrc is initial.
          if not wa_kna1-sperr is initial.
            message e351(f5) with wa_kna1-kunnr '0000' raising error.
          endif.
          if not wa_kna1-loevm is initial.
            message e001(f5a) with wa_kna1-kunnr '0000' raising error.
          endif.
          if not wa_kna1-nodel is initial.
            message e899(f5) with 'Bloqueio central de eliminação para registro mestre' raising error.
          endif.
        else.
          message e011(z01) with 'Cliente' p_cliente 'não localizado!' raising error.
        endif.

        if ( not p_empresa is initial ).
          select single * into wa_knb1 from knb1 where kunnr eq p_cliente and bukrs eq p_empresa.
          if sy-subrc is initial.
            if not wa_knb1-sperr is initial.
              message e351(f5) with wa_kna1-kunnr wa_knb1-bukrs raising error.
            endif.
*
            if not wa_knb1-loevm is initial.
              message e001(f5a) with wa_kna1-kunnr wa_knb1-bukrs raising error.
            endif.
          else.
            message e011(z01) with 'Cliente' p_cliente p_empresa 'não localizado!' raising error.
          endif.
        endif.
      else.
        message e011(z01) with 'Deve ser informado uma conta cliente!' raising error.
      endif.

    when 'K'.
*K  Fornecedores
      if not p_fornecedor is initial.

        select single * into wa_lfa1 from lfa1 where lifnr eq p_fornecedor.

        if sy-subrc is initial.
          if not wa_lfa1-sperr is initial.
            message e351(f5) with wa_lfa1-lifnr '0000' raising error.
          endif.
          if not wa_lfa1-loevm is initial.
            message e002(f5a) with wa_lfa1-lifnr '0000' raising error.
          endif.
          if not wa_lfa1-sperm is initial.
            message e022(me) with wa_lfa1-lifnr raising error.
          endif.
          if not wa_lfa1-nodel is initial.
            message e899(f5) with 'Bloqueio central de eliminação para registro mestre' raising error.
          endif.
        else.
          message e011(z01) with 'Fornecedor' p_fornecedor 'não localizado!' raising error.
        endif.

        if ( not p_empresa is initial ).
          select single * into wa_lfb1 from lfb1 where lifnr eq p_fornecedor and bukrs eq p_empresa.
          if sy-subrc is initial.
            if not wa_lfb1-sperr is initial.
              message e351(f5) with wa_lfb1-lifnr wa_lfb1-bukrs raising error.
            endif.
*
            if not wa_lfb1-loevm is initial.
              message e002(f5a) with wa_lfb1-lifnr wa_lfb1-bukrs raising error.
            endif.
          else.
            message e011(z01) with 'Fornecedor' p_fornecedor p_empresa 'não localizado!' raising error.
          endif.
        endif.

      else.
        message e011(z01) with 'Deve ser informado uma conta fornecedor!' raising error.
      endif.

    when 'M'.
*M  Material


    when 'S'.
*S  Contas do Razão
  endcase.

endfunction.
