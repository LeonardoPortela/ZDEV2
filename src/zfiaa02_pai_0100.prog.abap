*&---------------------------------------------------------------------*
*&  Include           ZFIAA02_PAI_0100
*&---------------------------------------------------------------------*


*----------------------------------------------------------------------*
*  MODULE pai_0100 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module pai_0100 input.
  create object: r_tipo_operacao,
                 r_utils.



  case sy-ucomm.
    when c_back.
      leave to screen 0.

    when c_canc or c_exit.
      leave program.

    when c_show_msg.
      r_utils->show_splitter_error( i_show  = x
                                    i_popup = space ).
    when c_save.
      case screen_principal.
        when 0140.
          refresh gt_msg_return.
          r_utils->validar_screen_0140( ).

*          op_mode = c_save.

          check gt_msg_return is initial.
          r_tipo_operacao->salvar_dados_0140( ).
        when others.
      endcase.

    when c_cad_imposto.

      call function 'AUTHORITY_CHECK_TCODE'
        exporting
          tcode  = 'ZAA13'
        exceptions
          ok     = 1
          not_ok = 2.

      if sy-subrc = 1.
        call transaction 'ZAA13'. "Tela de parametrização de vencimento de impostos.
      else.
        message e077(s#) with 'ZAA13'.
      endif.

    when c_clear.
      case screen_principal.
        when 0110.
          clear: gt_saida_0110, gt_msg_return, wl_mensagem.
        when 0140.
          clear: gt_saida_0140, gt_msg_return, wl_mensagem.
        when others.
      endcase.
    when others.
  endcase.
endmodule.                 " PAI_0100  INPUT
