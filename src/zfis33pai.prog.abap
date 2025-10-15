*&---------------------------------------------------------------------*
*&  Include           ZFIS33PAI
*&---------------------------------------------------------------------*

module user_command_0100.

  data: wl_refresh type c value 'X'.

  case sy-ucomm.
    when 'SAVE'.
      perform grava_dados.
    when 'F_SELECAO_EMPRESA'."Equalização ECC X HANA #108307 inicio  - SMC
*      PERFORM f_selecao_empresa.
    when 'F_SELECAO_EMP'.
*      PERFORM f_selecao_emp.
  endcase.
  "Equalização ECC X HANA #108307  fim - SMC

  if p_bukrs is initial.
    message i024(sd) with 'Informe a empresa!'.
  endif.

  if rd_cfop = 'X'.

    screen_cadastro = '0101'.

    if g_custom_container is initial.

      perform create_and_init_alv changing gt_fieldcat.

    else.
      perform seleciona_dados."Equalização ECC X HANA #108307   - SMC
    endif.

  elseif rd_filial_cc is not initial.

    screen_cadastro = '0102'.

*    CLEAR G_CUSTOM_CONTAINER2.

    if g_custom_container2 is initial.
      perform create_and_init_alv2 changing gt_fieldcat2.
    else.
      perform seleciona_dados."Equalização ECC X HANA #108307   - SMC
    endif.

  elseif rd_filial_ctb is not initial.
    screen_cadastro = '0103'.

    if g_custom_container3 is initial.
      perform create_and_init_alv3 changing gt_fieldcat3.

    else.
      perform seleciona_dados.

    endif.
  elseif rd_grupo_merc is not initial."Equalização ECC X HANA #108307 inicio  - SMC
    screen_cadastro = '0104'.

    if g_custom_container4 is initial.
      perform create_and_init_alv4 changing gt_fieldcat4."Equalização ECC X HANA #108307 fim  - SMC

    else.
      perform seleciona_dados."Equalização ECC X HANA #108307   - SMC

    endif.

  endif.

  perform f_refresh_alv.

endmodule.

module exit_screen.
  case sy-ucomm.
    when 'EXIT' or 'CANCEL'.
      leave to screen 0.
    when 'BACK'.
      leave to screen 0.
  endcase.
endmodule.



module seleciona_dados input.
  perform seleciona_dados.
endmodule.
