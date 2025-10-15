
MODULE user_command_0100 INPUT.
  PERFORM action_process.
ENDMODULE.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'T0100'.
  CREATE OBJECT lo_report.
  lo_report->get_data( ).

  IF r1 = 'X'. "Analítico
    lo_report->generate_output1( ).
  ELSEIF r2 = 'X'.
    lo_report->make_email( ).
    lo_report->generate_output2( ).
  ENDIF.

ENDMODULE.

FORM action_process.
  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.
ENDFORM.

FORM gera_table_header.

  DATA: it_table_header TYPE STANDARD TABLE OF string INITIAL SIZE 0,
        wa_string       TYPE string.

  wa_string = |<table BORDER="1" style="width:100%"><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |<tr><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |<th>Empresa</th><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

*  wa_string = |<th>Conta</th><!---->|.
*  APPEND wa_string TO it_table_header.
*  CLEAR:wa_string.

  wa_string = |<th>Conta</th><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |<th>Fornecedor</th><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |<th>Doc. compra</th><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |<th>Divisão</th><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |<th>Nº documento</th><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |<th>Data Lançamento</th><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |<th>Vencimento Líquido</th><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |<th>Moeda</th><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |<th>Montante em MI(R$)</th><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |<th>Montante em MI(USD)</th><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |<th>Tipo Doc</th><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |<th>CL</th><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |<th>Bloq. Pgto</th><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |<th>Texto</th><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |<th>Razão</th><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  wa_string = |</tr><!---->|.
  APPEND wa_string TO it_table_header.
  CLEAR:wa_string.

  CLEAR: ls_table_header.
  CONCATENATE LINES OF it_table_header INTO ls_table_header.

ENDFORM.

FORM gera_table_dados USING _bukrs TYPE bukrs _gsber TYPE gsber _bschl TYPE bschl.

  DATA: it_table_conteudo TYPE STANDARD TABLE OF string INITIAL SIZE 0,
        wa_string         TYPE string,
        vlr               TYPE bsak-dmbtr,
        vlr_txt           TYPE c LENGTH 20,
        lv_date           TYPE d,
        lv_date_out       TYPE char10.

  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) WHERE bukrs = _bukrs AND gsber = _gsber AND bschl = _bschl.

    wa_string = |<tr><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

    wa_string = |<td>{ <fs_saida>-bukrs }</td><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

*    wa_string = |<td>{ <fs_saida>-gkont }</td><!---->|.
*    APPEND wa_string TO it_table_conteudo.
*    CLEAR:wa_string.

    wa_string = |<td>{ <fs_saida>-lifnr }</td><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

    wa_string = |<td>{ <fs_saida>-name1 }</td><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.


    wa_string = |<td>{ <fs_saida>-ebeln }</td><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

    wa_string = |<td>{ <fs_saida>-gsber }</td><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

    wa_string = |<td>{ <fs_saida>-belnr }</td><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

    CLEAR: lv_date, lv_date_out.
    lv_date = <fs_saida>-budat.
    lv_date_out = |{ lv_date+6(2) }/{ lv_date+4(2) }/{ lv_date(4) }|.

    wa_string = |<td>{ lv_date_out }</td><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

    CLEAR: lv_date, lv_date_out.
    lv_date = <fs_saida>-bldat.
    lv_date_out = |{ lv_date+6(2) }/{ lv_date+4(2) }/{ lv_date(4) }|.

    wa_string = |<td>{ lv_date_out }</td><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

    wa_string = |<td>{ <fs_saida>-waers }</td><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

    CLEAR: vlr,vlr_txt.
    vlr = <fs_saida>-dmbtr.
    WRITE vlr TO vlr_txt.
    CONDENSE vlr_txt NO-GAPS.
    wa_string = |<td>{ vlr_txt }</td><!---->|.

    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

    CLEAR: vlr,vlr_txt.
    vlr = <fs_saida>-dmbe2.
    WRITE vlr TO vlr_txt.
    CONDENSE vlr_txt NO-GAPS.

    wa_string = |<td>{ vlr_txt }</td><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

    wa_string = |<td>{ <fs_saida>-blart }</td><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

    wa_string = |<td>{ <fs_saida>-bschl }</td><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

    wa_string = |<td>{ <fs_saida>-zlspr }</td><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

    wa_string = |<td>{ <fs_saida>-sgtxt }</td><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

    wa_string = |<td>{ <fs_saida>-hkont }</td><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

    wa_string = |</tr><!---->|.
    APPEND wa_string TO it_table_conteudo.
    CLEAR:wa_string.

  ENDLOOP.

  wa_string = |</table><!---->|.
  APPEND wa_string TO it_table_conteudo.
  CLEAR:wa_string.

  IF <fs_saida>-bschl = '29'.
    CLEAR: ls_table_conteudo29.
    CONCATENATE LINES OF it_table_conteudo INTO ls_table_conteudo29.
  ELSEIF <fs_saida>-bschl = '39'.
    CLEAR: ls_table_conteudo39.
    CONCATENATE LINES OF it_table_conteudo INTO ls_table_conteudo39.
  ENDIF.

ENDFORM.

FORM send .

ENDFORM.

FORM valida_execucao .

  IF sy-batch EQ abap_true.
    TRY.
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.


    CREATE OBJECT lo_report.
    lo_report->get_data( ).
    lo_report->make_email( ).

    IF it_mail IS NOT INITIAL.
      LOOP AT it_mail ASSIGNING FIELD-SYMBOL(<_send>).
        lo_report->send_email( ls_mail = <_send> ).
      ENDLOOP.
    ENDIF.

  ELSE.
    IF r1 = 'X' OR r2 = 'X'.
      CALL SELECTION-SCREEN '0100'.
    ENDIF.
  ENDIF.

ENDFORM.
