FUNCTION z_sd_print_boleto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(DOC_NUMERO) TYPE  J_1BDOCNUM
*"     REFERENCE(TIPO) TYPE  CHAR1 DEFAULT 'N'
*"     REFERENCE(HBKID) TYPE  HBKID DEFAULT 'BBRA'
*"     REFERENCE(INSTRUCOES) TYPE  ZFI_BOLETO OPTIONAL
*"----------------------------------------------------------------------
  DATA: vdoc_numero TYPE j_1bdocnum.
  CLEAR: p_email, t_hbkid .
  CLEAR: rb_imprimir.
  rb_email = c_x.
  t_doc_numero = doc_numero.
  wl_instrucoes = instrucoes.
  t_tipo = tipo.

  IF hbkid IS INITIAL OR hbkid = ''.
    t_hbkid = 'BBRA'. "HBKID. "US - 81799 - CBRAND
  ELSE.
    t_hbkid = hbkid.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = doc_numero
    IMPORTING
      output = vdoc_numero.
  CONCATENATE 'SOL.' vdoc_numero INTO p_xblnr.
  IF tipo NE 'N'.

    SELECT SINGLE kunnr FROM zsdt0051
      INTO @DATA(cliente)
      WHERE nro_sol_ov EQ @vdoc_numero.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE adrnr FROM kna1
        INTO @DATA(adrnr)
        WHERE kunnr EQ @cliente.
      IF sy-subrc IS INITIAL.
        SELECT SINGLE smtp_addr FROM adr6
          INTO p_email
          WHERE addrnumber EQ adrnr.
      ENDIF.
    ENDIF.

    CALL SCREEN 300 ENDING AT 51 8 STARTING AT 3 3.
  ELSE.
    PERFORM:
             f_seleciona_dados USING t_doc_numero t_tipo t_hbkid, " Form seleciona dados
*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
***             f_processa_dados USING t_tipo t_hbkid.
             f_processa_dados USING t_tipo t_hbkid wl_instrucoes.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610

    LOOP AT it_saida INTO wa_saida.
      PERFORM f_imprime_smart USING wa_saida p_email t_doc_numero t_hbkid.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
