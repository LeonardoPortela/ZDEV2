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
*| Este é um JOB (serviço) para atualização da ZLEST0079                     |*
*|                                                                           |*
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

REPORT  zlesj0009.

TABLES: zlest0079.

TYPES: BEGIN OF ty_zlest0061,
          dt_fatura	      TYPE zlest0061-dt_fatura,
          bukrs	          TYPE zlest0061-bukrs,
          werks	          TYPE zlest0061-werks,
          ano_viagem      TYPE zlest0061-ano_viagem,
          nr_viagem	      TYPE zlest0061-nr_viagem,
          nome_emb        TYPE zlest0061-nome_emb,
          cod_material    TYPE zlest0061-cod_material,
          tp_class        TYPE zlest0061-tp_class,
          cl_codigo	      TYPE zlest0061-cl_codigo,
          peso_vinculado  TYPE zlest0061-peso_vinculado,
          peso_chegada    TYPE zlest0061-peso_chegada,
          dt_chegada      TYPE zlest0061-dt_chegada,
          nr_ov	          TYPE zlest0061-nr_ov,
          docnum          TYPE zlest0061-docnum,
       END OF ty_zlest0061.

DATA: var_data     TYPE sy-datum. "Variavel para guardar a data de 30 dias atras.
DATA: var_data_dia TYPE sy-datum. "Variavel para guardar a data de -1 dias atras.

DATA: gt_zlest0061 TYPE TABLE OF ty_zlest0061, "Frete Aquaviário - Ordem de Venda
      gw_zlest0061 TYPE ty_zlest0061. "Frete Aquaviário - Ordem de Venda

DATA: gt_zlest0056 TYPE TABLE OF zlest0056, "Frete Aquaviário - Viagem
      gw_zlest0056 TYPE zlest0056. "Frete Aquaviário - Viagem

DATA: gt_kna1 TYPE TABLE OF kna1, "Mestre de clientes (parte geral)
      gw_kna1 TYPE kna1. "Mestre de clientes (parte geral)

DATA: gt_zlest0060 TYPE TABLE OF zlest0060,
      gw_zlest0060 TYPE zlest0060.

DATA: gt_lips TYPE TABLE OF lips,
      gw_lips TYPE lips.

DATA: gw_t001k TYPE t001k,
      gw_zlest0093 TYPE zlest0093.

DATA: gw_zlest0079 TYPE zlest0079.


DATA: var_bwkey TYPE t001k-bwkey.

CLEAR: var_data_dia, var_data.

var_data = sy-datum - 30.

"Selecionar todos os conhecimentos do Aquaviário.
SELECT dt_fatura bukrs werks ano_viagem nr_viagem nome_emb cod_material tp_class cl_codigo peso_vinculado peso_chegada dt_chegada nr_ov docnum
   FROM zlest0061
  INTO TABLE gt_zlest0061
WHERE ( dt_fatura >= var_data AND dt_fatura <= sy-datum ).

CHECK NOT gt_zlest0061[] IS INITIAL.

SELECT * FROM zlest0056
  INTO TABLE gt_zlest0056
  FOR ALL ENTRIES IN gt_zlest0061
WHERE bukrs      EQ gt_zlest0061-bukrs
  AND werks      EQ gt_zlest0061-werks
  AND ano_viagem EQ gt_zlest0061-ano_viagem
  AND nr_viagem  EQ gt_zlest0061-nr_viagem.

SELECT * FROM kna1
  INTO TABLE gt_kna1
  FOR ALL ENTRIES IN gt_zlest0061
WHERE kunnr EQ gt_zlest0061-cl_codigo.

SELECT * FROM zlest0060
  INTO TABLE gt_zlest0060
  FOR ALL ENTRIES IN gt_zlest0061
WHERE docnum EQ gt_zlest0061-docnum.

SELECT * FROM lips
  INTO TABLE gt_lips
  FOR ALL ENTRIES IN gt_zlest0060
WHERE vbeln EQ gt_zlest0060-doc_rem.

var_data_dia = sy-datum - 1.

LOOP AT gt_zlest0061 INTO gw_zlest0061.

  "Carga
  IF ( gw_zlest0061-dt_fatura >= var_data ) AND ( gw_zlest0061-dt_fatura <= var_data_dia ).

    CONCATENATE gw_zlest0061-werks gw_zlest0061-ano_viagem gw_zlest0061-nr_viagem gw_zlest0061-docnum INTO gw_zlest0079-chave.

    "Verificar se já existe a chave e deletar para carga/descarga.
    PERFORM: deletar_chave USING gw_zlest0079-chave
                                 'CARGA'.

    gw_zlest0079-data_base = sy-datum.
    gw_zlest0079-idinter   = 'L4'.
    gw_zlest0079-tp_movi   = 'S'.
    gw_zlest0079-tp_reg    = '20'.

    gw_zlest0079-dcl      = space.
    gw_zlest0079-seriedcl = space.
    gw_zlest0079-id       = '1'.

    READ TABLE gt_kna1 INTO gw_kna1 WITH KEY kunnr = gw_zlest0061-cl_codigo.

    CASE gw_kna1-ktokd.
      WHEN: 'ZCIC'.

        CLEAR: var_bwkey.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = gw_zlest0061-cl_codigo
          IMPORTING
            output = var_bwkey.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = var_bwkey
          IMPORTING
            output = var_bwkey.



        SELECT SINGLE * FROM t001k INTO gw_t001k WHERE bwkey EQ var_bwkey.
        gw_zlest0079-empresa = gw_t001k-bukrs.
        gw_zlest0079-werks   = gw_t001k-bwkey.

        READ TABLE gt_zlest0060 INTO gw_zlest0060 WITH KEY docnum = gw_zlest0061-docnum.
        IF sy-subrc = 0.
          READ TABLE gt_lips INTO gw_lips WITH KEY vbeln = gw_zlest0060-doc_rem.
          IF sy-subrc = 0.
            gw_zlest0079-produto   = gw_lips-matnr.
          ENDIF.
        ENDIF.


      WHEN OTHERS.
        SELECT SINGLE * FROM zlest0093 INTO gw_zlest0093 WHERE stcd1 EQ gw_kna1-stcd1.
        gw_zlest0079-empresa = gw_zlest0093-bukrs.
        gw_zlest0079-werks   = gw_zlest0093-werks.
        gw_zlest0079-produto = gw_zlest0061-cod_material.
    ENDCASE.

    READ TABLE gt_zlest0056 INTO gw_zlest0056 WITH KEY bukrs      = gw_zlest0061-bukrs
                                                       werks      = gw_zlest0061-werks
                                                       ano_viagem = gw_zlest0061-ano_viagem
                                                       nr_viagem  = gw_zlest0061-nr_viagem.


    gw_zlest0079-origem         = gw_zlest0056-po_embarque.
    gw_zlest0079-tipo_produto   = gw_zlest0061-tp_class.
    gw_zlest0079-destino        = gw_zlest0056-po_destino.
    gw_zlest0079-data_saida     = gw_zlest0061-dt_fatura.

    gw_zlest0079-quantidade     = gw_zlest0061-peso_vinculado.
    gw_zlest0079-unidade_medida = 'KG'.
    gw_zlest0079-vbeln          = gw_zlest0061-nr_ov.
    gw_zlest0079-modal          = '03'.

    INSERT INTO zlest0079 VALUES gw_zlest0079.

    COMMIT WORK.

    "Transito
    IF ( gw_zlest0061-dt_fatura < sy-datum ) AND ( gw_zlest0061-dt_fatura > var_data ) AND ( gw_zlest0061-dt_fatura NE var_data_dia ) AND ( gw_zlest0061-dt_chegada IS INITIAL ).


      CLEAR: gw_zlest0079.

      gw_zlest0079-data_base      = sy-datum.
      gw_zlest0079-idinter        = 'L4'.
      gw_zlest0079-tp_movi        = 'S'.
      gw_zlest0079-tp_reg         = '20'.
      CONCATENATE gw_zlest0061-werks gw_zlest0061-ano_viagem gw_zlest0061-nr_viagem gw_zlest0061-docnum INTO gw_zlest0079-chave.

      gw_zlest0079-id             = '2'.

      READ TABLE gt_kna1 INTO gw_kna1 WITH KEY kunnr = gw_zlest0061-cl_codigo.

      CASE gw_kna1-ktokd.
        WHEN: 'ZCIC'.

          CLEAR: var_bwkey.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = gw_zlest0061-cl_codigo
            IMPORTING
              output = var_bwkey.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = var_bwkey
            IMPORTING
              output = var_bwkey.

          SELECT SINGLE * FROM t001k INTO gw_t001k WHERE bwkey EQ var_bwkey.

          gw_zlest0079-empresa = gw_t001k-bukrs.
          gw_zlest0079-werks   = gw_t001k-bwkey.

          READ TABLE gt_zlest0060 INTO gw_zlest0060 WITH KEY docnum = gw_zlest0061-docnum.
          IF sy-subrc = 0.
            READ TABLE gt_lips INTO gw_lips WITH KEY vbeln = gw_zlest0060-doc_rem.
            IF sy-subrc = 0.
              gw_zlest0079-produto   = gw_lips-matnr.
            ENDIF.
          ENDIF.

        WHEN OTHERS.
          SELECT SINGLE * FROM zlest0093 INTO gw_zlest0093 WHERE stcd1 EQ gw_kna1-stcd1.
          gw_zlest0079-empresa = gw_zlest0093-bukrs.
          gw_zlest0079-werks   = gw_zlest0093-werks.
          gw_zlest0079-produto        = gw_zlest0061-cod_material.
      ENDCASE.

      READ TABLE gt_zlest0056 INTO gw_zlest0056 WITH KEY bukrs      = gw_zlest0061-bukrs
                                                         werks      = gw_zlest0061-werks
                                                         ano_viagem = gw_zlest0061-ano_viagem
                                                         nr_viagem  = gw_zlest0061-nr_viagem.


      gw_zlest0079-origem         = gw_zlest0056-po_embarque.
      gw_zlest0079-tipo_produto   = gw_zlest0061-tp_class.
      gw_zlest0079-destino        = gw_zlest0056-po_destino.
      gw_zlest0079-data_saida     = gw_zlest0061-dt_fatura.

      gw_zlest0079-quantidade     = gw_zlest0061-peso_vinculado.
      gw_zlest0079-unidade_medida = 'KG'.
      gw_zlest0079-vbeln          = gw_zlest0061-nr_ov.
      gw_zlest0079-modal          = '03'.

      INSERT INTO zlest0079 VALUES gw_zlest0079.

      COMMIT WORK.


      "Descarga
    ELSEIF ( gw_zlest0061-dt_fatura < sy-datum ) AND ( gw_zlest0061-dt_fatura > var_data ) AND NOT ( gw_zlest0061-dt_chegada IS INITIAL ) .

      CONCATENATE gw_zlest0061-werks gw_zlest0061-ano_viagem gw_zlest0061-nr_viagem gw_zlest0061-docnum INTO gw_zlest0079-chave.

      "Verificar se já existe a chave e deletar para carga/descarga.
      PERFORM: deletar_chave USING gw_zlest0079-chave
                                   'DESCARGA'.

      gw_zlest0079-data_base      = sy-datum.
      gw_zlest0079-idinter        = 'L5'.
      gw_zlest0079-tp_movi        = 'E'.
      gw_zlest0079-tp_reg         = '20'.

      gw_zlest0079-id             = '3'.

      READ TABLE gt_kna1 INTO gw_kna1 WITH KEY kunnr = gw_zlest0061-cl_codigo.

      CASE gw_kna1-ktokd.
        WHEN: 'ZCIC'.

          CLEAR: var_bwkey.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = gw_zlest0061-cl_codigo
            IMPORTING
              output = var_bwkey.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = var_bwkey
            IMPORTING
              output = var_bwkey.

          SELECT SINGLE * FROM t001k INTO gw_t001k WHERE bwkey EQ var_bwkey.
          gw_zlest0079-empresa = gw_t001k-bukrs.
          gw_zlest0079-werks   = gw_t001k-bwkey.

          READ TABLE gt_zlest0060 INTO gw_zlest0060 WITH KEY docnum = gw_zlest0061-docnum.
          IF sy-subrc = 0.
            READ TABLE gt_lips INTO gw_lips WITH KEY vbeln = gw_zlest0060-doc_rem.
            IF sy-subrc = 0.
              gw_zlest0079-produto   = gw_lips-matnr.
            ENDIF.
          ENDIF.

        WHEN OTHERS.
          SELECT SINGLE * FROM zlest0093 INTO gw_zlest0093 WHERE stcd1 EQ gw_kna1-stcd1.
          gw_zlest0079-empresa = gw_zlest0093-bukrs.
          gw_zlest0079-werks   = gw_zlest0093-werks.
          gw_zlest0079-produto        = gw_zlest0061-cod_material.
      ENDCASE.

      READ TABLE gt_zlest0056 INTO gw_zlest0056 WITH KEY bukrs      = gw_zlest0061-bukrs
                                                         werks      = gw_zlest0061-werks
                                                         ano_viagem = gw_zlest0061-ano_viagem
                                                         nr_viagem  = gw_zlest0061-nr_viagem.


      gw_zlest0079-origem         = gw_zlest0056-po_embarque.
      gw_zlest0079-tipo_produto   = gw_zlest0061-tp_class.
      gw_zlest0079-destino        = gw_zlest0056-po_destino.
      gw_zlest0079-data_saida     = gw_zlest0061-dt_fatura.

      gw_zlest0079-quantidade     = gw_zlest0061-peso_vinculado.
      gw_zlest0079-unidade_medida = 'KG'.
      gw_zlest0079-vbeln          = gw_zlest0061-nr_ov.
      gw_zlest0079-modal          = '03'.
      gw_zlest0079-data_chegada   = gw_zlest0061-dt_chegada.

      INSERT INTO zlest0079 VALUES gw_zlest0079.

      COMMIT WORK.
    ENDIF.
  ENDIF.

  CLEAR: gw_zlest0079, gw_zlest0060, gw_zlest0061, gw_lips, gw_kna1, gw_zlest0056, gw_zlest0093.
ENDLOOP.
*&---------------------------------------------------------------------*
*&      Form  DELETAR_CHAVE
*&---------------------------------------------------------------------*
FORM deletar_chave  USING p_chave TYPE zlest0079-chave
                          p_tipo  TYPE any.

  DATA: lw_zlest0079 TYPE zlest0079.
  "Verificar se esta na tabeal ZLEST0079

  SELECT SINGLE * FROM zlest0079 WHERE chave EQ p_chave.
  IF ( sy-subrc EQ 0 ).

    CASE p_tipo.
      WHEN: 'CARGA'.
        DELETE FROM zlest0079 WHERE chave   EQ p_chave
                                AND idinter EQ 'L4'
                                AND tp_movi EQ 'S'
                                AND tp_reg  EQ '20'
                                AND id      EQ '1'.
        COMMIT WORK.

      WHEN: 'DESCARGA'.
        DELETE FROM zlest0079 WHERE chave   EQ p_chave
                                AND idinter EQ 'L5'
                                AND tp_movi EQ 'E'
                                AND tp_reg  EQ '20'
                                AND id      EQ '3'.

        COMMIT WORK.
    ENDCASE.
  ENDIF.

ENDFORM.                    " DELETAR_CHAVE
