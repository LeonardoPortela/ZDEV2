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
*| JOB para recuperar dados da ZSDT0001 e alimentar a tabela ZLEST00087      |*
*| para as descargas de terceiro no terminal da Hermasa Porto Velho          |*
*/===========================================================================\*

*/===========================================================================\*
*|  Desenvolvedor:                                                           |*
*|    + Victor Hugo Souza Nunes ( victor.hugo@grupomaggi.com.br )            |*
*|                                                                           |*
*|  Tester:                                                                  |*
*|    + Leila Mara Vançan ( leila.vançan@grupomaggi.com.br )                 |*
*|  Changelog:                                                               |*
*|                                                                           |*
*/===========================================================================\*

REPORT  zlesj0010.
TABLES: zlest0087.

DATA: var_mov    TYPE c LENGTH 1 VALUE 'E', "Variavel atribuindo o tipo E = Entrada
      var_bukrs  TYPE c LENGTH 4 VALUE '0010', "Variavel atribuindo o valor da empresa 0010 (Hermasa)
      var_branch TYPE c LENGTH 4 VALUE '1002'. "Variavel atribuindo o centro Porto Velho.


DATA: gt_zsdt0001    TYPE TABLE OF zsdt0001, "Informações para preenchimento dos dados de remessa. (OPUS)
      gw_zsdt0001    TYPE zsdt0001, "Informações para preenchimento dos dados de remessa. (OPUS)
      gt_zlest0093   TYPE TABLE OF zlest0093, "De X Para - Empresa Centro Terceiros
      gw_zlest0093   TYPE zlest0093, "De X Para - Empresa Centro Terceiros
      gt_lfa1        TYPE TABLE OF lfa1, "Mestre de fornecedores (parte geral)
      gw_lfa1        TYPE lfa1, "Mestre de fornecedores (parte geral)
      gw_lfa1_branch TYPE lfa1.

DATA: gw_input_87 TYPE zlest0087.

DATA: var_data     TYPE sy-datum. "Variavel para guardar a data de 30 dias atras.
DATA: var_lifnr    TYPE lfa1-lifnr.
DATA: var_refkey   TYPE zlest0087-id_refkey.


CLEAR: var_data.

var_data = sy-datum - 30.

"Informações para preenchimento dos dados de remessa. (OPUS)
SELECT * FROM zsdt0001
  INTO TABLE gt_zsdt0001
WHERE tp_movimento   EQ var_mov
  AND bukrs          EQ var_bukrs
  AND branch         EQ var_branch
  AND ( dt_movimento >= var_data AND dt_movimento <= sy-datum ).


CHECK NOT gt_zsdt0001[] IS INITIAL. "Caso tenha encontrado algum registro continuar o processo.

"recuperar dados Mestre de fornecedores (parte geral) comparando o LIFNR da ZSDT0001.
SELECT * FROM lfa1
  INTO TABLE gt_lfa1
  FOR ALL ENTRIES IN gt_zsdt0001
WHERE lifnr EQ gt_zsdt0001-parid.


CHECK NOT gt_lfa1[] IS INITIAL.  ""Caso tenha encontrado algum registro continuar o processo.

SELECT * FROM zlest0093
  INTO TABLE gt_zlest0093
  FOR ALL ENTRIES IN gt_lfa1
WHERE stcd1 EQ gt_lfa1-stcd1.

CHECK NOT gt_zlest0093[] IS INITIAL.  ""Caso tenha encontrado algum registro continuar o processo.

CLEAR:  var_refkey.
CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
  EXPORTING
    object           = 'ZSEQ_LES19'
  EXCEPTIONS
    foreign_lock     = 1
    object_not_found = 2
    system_failure   = 3
    OTHERS           = 4.

CALL FUNCTION 'NUMBER_GET_NEXT'
  EXPORTING
    nr_range_nr             = '1'
    object                  = 'ZSEQ_LES19'
    quantity                = '00000000000000000001'
    ignore_buffer           = 'X'
  IMPORTING
    number                  = var_refkey
  EXCEPTIONS
    interval_not_found      = 1
    number_range_not_intern = 2
    object_not_found        = 3
    quantity_is_0           = 4
    quantity_is_not_1       = 5
    interval_overflow       = 6
    buffer_overflow         = 7
    OTHERS                  = 8.

* Desbloqueia o objeto de numeração
CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
  EXPORTING
    object           = 'ZSEQ_LES19'
  EXCEPTIONS
    object_not_found = 1
    OTHERS           = 2.


LOOP AT gt_zsdt0001 INTO gw_zsdt0001.

  READ TABLE gt_lfa1 INTO gw_lfa1 WITH KEY lifnr = gw_zsdt0001-parid.
  IF ( sy-subrc NE 0 ).
    CONTINUE.
  ENDIF.

  READ TABLE gt_zlest0093 INTO gw_zlest0093 WITH KEY stcd1 = gw_lfa1-stcd1.
  IF ( sy-subrc NE 0 ).
    CONTINUE.
  ENDIF.

  CONCATENATE gw_zlest0093-bukrs '-' gw_zlest0093-werks '-' gw_zsdt0001-nfnum INTO gw_input_87-chave.
  PERFORM: deletar_chave USING gw_input_87-chave.

  gw_input_87-idinter   = 'L1'.
  gw_input_87-tp_movi   =  var_mov.
  gw_input_87-tp_reg    =  '30'.
  gw_input_87-id_refkey = var_refkey.

  gw_input_87-bukrs            = gw_zlest0093-bukrs.
  gw_input_87-werks            = gw_zlest0093-werks.
  gw_input_87-nr_nf            = gw_zsdt0001-nfnum.
  gw_input_87-serie            = gw_zsdt0001-series.
  gw_input_87-lifnr            = gw_zsdt0001-parid.

  gw_input_87-peso_nf          = gw_zsdt0001-peso_fiscal.
  gw_input_87-descarga_rodo    = gw_zsdt0001-peso_liq.
  gw_input_87-dt_descarga      = gw_zsdt0001-dt_movimento.
  gw_input_87-produto          = gw_zsdt0001-matnr.
  gw_input_87-nfe              = gw_zsdt0001-nfe.

  gw_input_87-dtaenvio         = sy-datum.
  gw_input_87-horaenvio        = sy-uzeit.
  gw_input_87-status           = 'P'.
  gw_input_87-id_frete         = 'T'.
  gw_input_87-uname            =  'R3JOB'.
  gw_input_87-erdat            = sy-datum.
  gw_input_87-erzet            = sy-uzeit.


  CLEAR: var_lifnr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gw_zsdt0001-branch
    IMPORTING
      output = var_lifnr.

  SELECT SINGLE * FROM lfa1 INTO gw_lfa1_branch WHERE lifnr EQ var_lifnr.
  gw_input_87-cnpj_prest_serv  = gw_lfa1_branch-stcd1.
  gw_input_87-empresa          = gw_lfa1_branch-mcod1.

  CASE gw_zsdt0001-tp_transgenia.
    WHEN: 'CO'.
      gw_input_87-lgort  =  'C001'.
    WHEN OTHERS.
      gw_input_87-lgort  =  'T001'.
  ENDCASE.



  IF NOT ( gw_input_87-id_refkey IS INITIAL ).
    INSERT INTO zlest0087 VALUES gw_input_87.
  ENDIF.

  CLEAR: gw_zsdt0001, gw_zlest0093, gw_lfa1, gw_lfa1_branch, gw_input_87.
ENDLOOP.
*&---------------------------------------------------------------------*
*&      Form  DELETAR_CHAVE
*&---------------------------------------------------------------------*
FORM deletar_chave  USING    p_chave TYPE zlest0087-chave.

  DATA: gl_zlest0087 TYPE zlest0087.

  CLEAR: gl_zlest0087.
  SELECT SINGLE * FROM zlest0087 INTO gl_zlest0087 WHERE chave EQ p_chave.
  IF ( sy-subrc EQ 0 ).

    DELETE FROM zlest0087 WHERE chave EQ p_chave.
    COMMIT WORK.

  ENDIF.
ENDFORM.                    " DELETAR_CHAVE
