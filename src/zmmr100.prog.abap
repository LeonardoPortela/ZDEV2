*/===========================================================================\*
*|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
*|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
*|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
*|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
*|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
*|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
*| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
*/===========================================================================\*

*/===========================================================================\*
*| Descrição:                                                                |*
*| JOB - Para enviar o e-mail da seleção de dados do contrato                |*
*/===========================================================================\*

*/===========================================================================\*
*|  Desenvolvedor:                                                           |*
*|    + Victor Hugo Souza Nunes ( victor.hugo@grupomaggi.com.br )            |*
*|                                                                           |*
*|  Tester:                                                                  |*
*|    + Marcos Santos ( marcos.santos@grupomaggi.com.br )                    |*
*/===========================================================================\*
REPORT  zmmr100.

***********************
* Internal Table
***********************
DATA: gt_ekko TYPE TABLE OF ekko.
*      gt_t024 TYPE TABLE OF t024.
*      gt_lfa1 TYPE TABLE OF lfa1.



***********************
* Work Area
***********************
DATA: gw_ekko TYPE ekko.
*      gw_t024 TYPE t024,
*      gw_lfa1 TYPE lfa1.

***********************
* Estrutura para o e-mail.
***********************
DATA: gt_destinatario TYPE STANDARD TABLE OF somlrec90 WITH HEADER LINE,
      gt_assunto      TYPE sodocchgi1,
      gt_texto        TYPE STANDARD TABLE OF soli WITH HEADER LINE.

***********************
* Objetos
***********************
DATA: obj_zcl_util TYPE REF TO zcl_util.

***********************
* Variavel
***********************
DATA: var_data    TYPE sy-datum,
      var_data_i  TYPE sy-datum,
      vlr_string  TYPE string,
      var_data_br TYPE c LENGTH 10.

**********************************************************************
* Fluxo Principal do Programa.
**********************************************************************

"
var_data_i = sy-datum - 10.
var_data   = sy-datum + 45.

"Cabeçalho do documento de compra
SELECT * FROM ekko
  INTO TABLE gt_ekko
WHERE bstyp EQ 'K'
  AND ( kdate >= sy-datum AND kdate <= var_data  ).

"Grupos de compra
SELECT ekgrp, eknam, smtp_addr
  FROM t024
  INTO TABLE @DATA(gt_t024)
  FOR ALL ENTRIES IN @gt_ekko
WHERE ekgrp EQ @gt_ekko-ekgrp.
SORT gt_t024[] BY ekgrp ASCENDING.

"Mestre de fornecedores (parte geral)
SELECT lifnr, name1 FROM lfa1
  INTO TABLE @DATA(gt_lfa1)
  FOR ALL ENTRIES IN @gt_ekko
WHERE lifnr EQ @gt_ekko-lifnr.
SORT gt_lfa1[] BY lifnr ASCENDING.

FREE: obj_zcl_util.
CREATE OBJECT obj_zcl_util.


LOOP AT gt_ekko INTO gw_ekko.


  CONCATENATE 'SAP Contr.' gw_ekko-ebeln INTO gt_assunto-obj_name.
  CONCATENATE 'SAP - Vencimento Contrato ' gw_ekko-ebeln INTO gt_assunto-obj_descr.
  gt_assunto-obj_langu = sy-langu.


  gt_texto = '<!DOCTYPE html>'.
  APPEND gt_texto.
  gt_texto = '<html>'.
  APPEND gt_texto.
  gt_texto = '<head>'.
  APPEND gt_texto.
  gt_texto = '<style type="text/css">'.
  APPEND gt_texto.

  gt_texto = ' #tdEstilo {  background-color: #BAEEAD; }'.
  APPEND gt_texto.
  gt_texto = 'table { border: 1px solid black; font-size: 16px;  padding: 0px;  border-collapse: collapse; }'.
  APPEND gt_texto.
  gt_texto = '#tdDados { font-weight: bold; }'.
  APPEND gt_texto.

  gt_texto = '</style>'.
  APPEND gt_texto.
  gt_texto = '</head>'.

  APPEND gt_texto.
  gt_texto = '<body lang="pt-br">'.
  APPEND gt_texto.

  gt_texto = '<table>'.
  APPEND gt_texto.

  CONCATENATE '<tr><td id ="tdEstilo">Contrato:</td><td id="tdDados">' gw_ekko-ebeln '</td></tr>' INTO gt_texto.
  APPEND gt_texto.

  obj_zcl_util->conv_data_us_br( EXPORTING i_data  = gw_ekko-bedat
                                           i_opcao = '.'
                                 RECEIVING e_data = var_data_br ).


  CONCATENATE '<tr><td id ="tdEstilo">Data:</td><td id="tdDados">' var_data_br '</td></tr>'     INTO gt_texto.
  APPEND gt_texto.

  vlr_string = gw_ekko-ktwrt.
  CONCATENATE '<tr><td id ="tdEstilo">Valor:</td><td id="tdDados">' vlr_string '</td></tr>'       INTO gt_texto.
  APPEND gt_texto.

  CONCATENATE '<tr><td id ="tdEstilo">Moeda:</td><td id="tdDados">' gw_ekko-waers '</td></tr>'    INTO gt_texto.
  APPEND gt_texto.

*  READ TABLE gt_lfa1 INTO gw_lfa1 WITH KEY lifnr = gw_ekko-lifnr BINARY SEARCH.
  READ TABLE gt_lfa1 ASSIGNING FIELD-SYMBOL(<lfs_lfa1>) WITH KEY lifnr = gw_ekko-lifnr BINARY SEARCH.
  CONCATENATE '<tr><td id ="tdEstilo">Fornecedor:</td><td id="tdDados">' gw_ekko-lifnr '-' <lfs_lfa1>-name1 '</td></tr>'  INTO gt_texto.
  APPEND gt_texto.

  CLEAR: var_data_br.
  obj_zcl_util->conv_data_us_br( EXPORTING i_data  = gw_ekko-kdate
                                           i_opcao = '.'
                                 RECEIVING e_data = var_data_br ).

  CONCATENATE '<tr><td id ="tdEstilo">Data Vencimento:</td><td id="tdDados">' var_data_br '</td></tr>' INTO gt_texto.
  APPEND gt_texto.

*  READ TABLE gt_t024 INTO gw_t024 WITH KEY ekgrp = gw_ekko-ekgrp BINARY SEARCH.
  READ TABLE gt_t024 ASSIGNING FIELD-SYMBOL(<lfs_t024>) WITH KEY ekgrp = gw_ekko-ekgrp BINARY SEARCH.
  CONCATENATE '<tr><td id ="tdEstilo">Comprador:</td><td id="tdDados">' gw_ekko-ekgrp '-' <lfs_t024>-eknam '</td></tr>' INTO gt_texto.
  APPEND gt_texto.

  gt_texto = '</table>'.
  APPEND gt_texto.

  gt_texto = '</body>'.
  APPEND gt_texto.
  gt_texto = '</html>'.
  APPEND gt_texto.

  gt_destinatario-rec_type = 'U'.
  gt_destinatario-receiver = <lfs_t024>-smtp_addr.
  APPEND gt_destinatario.

  "Enviar E-mail
  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = gt_assunto
      document_type              = 'HTM'
    TABLES
      object_content             = gt_texto
      receivers                  = gt_destinatario
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF ( sy-subrc = 0 ).
    COMMIT WORK.
  ENDIF.

**  IF ( SY-SUBRC EQ 0 ).
**    COMMIT WORK.
***    SUBMIT RSCONN01 WITH MODE = 'INT' AND RETURN.
**  ENDIF.

  CLEAR: gw_ekko, vlr_string. "gw_t024. "gw_lfa1
  CLEAR: gt_assunto, gt_texto, gt_destinatario.

ENDLOOP.
