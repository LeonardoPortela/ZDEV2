class ZCL_SE_WL_MANUTENCAO_ROMANEIO definition
  public
  final
  create public .

public section.

  interfaces ZIF_SOFT_EXPERT_WS_INJECT .

  data AT_CARGA type ref to ZIF_CARGA .
  data AT_CARGA_ORIGINAL type ref to ZIF_CARGA .
  data OBS_ALTERACAO type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SE_WL_MANUTENCAO_ROMANEIO IMPLEMENTATION.


  METHOD ZIF_SOFT_EXPERT_WS_INJECT~GET_ATTRIBUTES_VALUES.
    CLEAR: E_XML_ATRIBUTOS.
    R_INSTANCE = ME.
  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WS_INJECT~GET_ENTITYS_CHILD_VALUES.

    DATA: DS_VALOR TYPE STRING.

    R_INSTANCE = ME.

    TRY .
        ME->AT_CARGA->GET_INFO_ALV_APRESENTACAO( IMPORTING E_APRESENTACAO = DATA(E_APRESENTACAO)
         )->GET_ROMANEIO_SAIDA(
              EXPORTING I_ID_CARGA  = ME->AT_CARGA->CARGA-ID_CARGA
              IMPORTING E_ROMANEIOS = DATA(E_ROMANEIOS_SAIDA)
         ).

        ME->AT_CARGA_ORIGINAL->GET_INFO_ALV_APRESENTACAO( IMPORTING E_APRESENTACAO = DATA(E_APRESENTACAO_ORIGINAL)
         )->GET_ROMANEIO_SAIDA(
              EXPORTING I_ID_CARGA  = ME->AT_CARGA_ORIGINAL->CARGA-ID_CARGA
              IMPORTING E_ROMANEIOS = DATA(E_ROMANEIOS_SAIDA_ORIGINAL)
         ).

      CATCH ZCX_ORDEM_CARREGAMENTO.    "
      CATCH ZCX_CARGA.    "
    ENDTRY.

    READ TABLE E_APRESENTACAO-NOTAS INDEX I_INDEX INTO DATA(WA_NOTA).
    READ TABLE E_APRESENTACAO_ORIGINAL-NOTAS INDEX I_INDEX INTO DATA(WA_NOTA_ORG).

    LOOP AT I_WORKFLOW->AT_ENTITYS_ATRIBUTES INTO DATA(WA_ENTITYS_ATRIBUTES_X_SAP)
        WHERE ENTITYID EQ I_ENTITYID.

      CLEAR: DS_VALOR.

      CASE WA_ENTITYS_ATRIBUTES_X_SAP-TABNAME.

        WHEN 'ZSDT0001NT'.

*Tipo de Entrada                GRID  DYNGSAMR  DS_ENTRADA          tipodeentrada1  tipodeentrada2
*ID Fornecedor                  GRID  DYNGSAMR  ID_FORNECEDOR       idfornecedor1   idfornecedor2
*Nome Fornecedor                GRID  DYNGSAMR  DS_FORNECEDOR       nomefornecedor1 nomefornecedor2
*Modelo NF (eletronica ou não)  GRID  DYNGSAMR  ID_MOD_FISCAL       modelonfelet1   modelonfelet2
*Numero da  NF                  GRID  DYNGSAMR  NR_NOTA             numerodanf1     numerodanf2
*Quantidade NF                  GRID  DYNGSAMR  NR_QUANTIDADE       quantidadenf1   quantidadenf2
*Valor NF                       GRID  DYNGSAMR  NR_VALOR            valornf1        valornf2
*Data Emissão NF                GRID  DYNGSAMR  DT_EMISSAO          dataemissonf1   dataemissonf2
*Vencimento Formulário          GRID  DYNGSAMR  DT_VENCIMENTO_FORM  vencimentoform1 vencimentoform2
*CFOP                           GRID  DYNGSAMR  CFOP                cfop1           cfop2
*Série NF                       GRID  DYNGSAMR  NM_SERIE            srienf1         srienf2
*Chave NF eletrônica            GRID  DYNGSAMR  NR_CHAVE_NFE        chavenfeletr1   chavenfeletr2
*Entregue por                   GRID  DYNGSAMR  DS_ENTREGUE_POR     entreguepor1    entreguepor2
*Peso SubTotal do Caminhão      GRID  DYNGSAMR  NM_PESO_SUBTOTAL    pesosubtotald1  pesosubtotald2

          CASE WA_ENTITYS_ATRIBUTES_X_SAP-FIELDNAME.
            WHEN 'AL_ID_ENTRADA'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_ID_ENTRADA ).
            WHEN 'AL_ID_FORNECEDOR'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_ID_FORNECEDOR ).
            WHEN 'AL_ID_MOD_FISCAL'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_ID_MOD_FISCAL ).
            WHEN 'AL_NR_NOTA'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_NOTA ).
            WHEN 'AL_NM_SERIE'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NM_SERIE ).
            WHEN 'AL_NR_QUANTIDADE'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_QUANTIDADE ).
            WHEN 'AL_NR_VALOR'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_VALOR ).
            WHEN 'AL_DT_EMISSAO'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_DT_EMISSAO ).
            WHEN 'AL_DT_VENCIMENTO_FORM'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_DT_VENCIMENTO_FORM ).
            WHEN 'AL_CFOP'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_CFOP ).
            WHEN 'AL_NR_CHAVE_NFE'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_CHAVE_NFE ).
            WHEN 'AL_ID_ENTREGUE_POR'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_ID_ENTREGUE_POR ).


            WHEN 'DS_ENTRADA'.
              DS_VALOR = CONV #( WA_NOTA_ORG-DS_ENTRADA ).
            WHEN 'DS_ENTRADA_2'.
              DS_VALOR = CONV #( WA_NOTA-DS_ENTRADA ).
            WHEN 'ID_FORNECEDOR'.
              DS_VALOR = CONV #( WA_NOTA_ORG-ID_FORNECEDOR ).
            WHEN 'ID_FORNECEDOR_2'.
              DS_VALOR = CONV #( WA_NOTA-ID_FORNECEDOR ).
            WHEN 'DS_FORNECEDOR'.
              DS_VALOR = CONV #( WA_NOTA_ORG-DS_FORNECEDOR ).
            WHEN 'DS_FORNECEDOR_2'.
              DS_VALOR = CONV #( WA_NOTA-DS_FORNECEDOR ).
            WHEN 'ID_MOD_FISCAL'.
              DS_VALOR = CONV #( WA_NOTA_ORG-ID_MOD_FISCAL ).
            WHEN 'ID_MOD_FISCAL_2'.
              DS_VALOR = CONV #( WA_NOTA-ID_MOD_FISCAL ).
            WHEN 'NR_NOTA'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NR_NOTA ).
            WHEN 'NR_NOTA_2'.
              DS_VALOR = CONV #( WA_NOTA-NR_NOTA ).
            WHEN 'NR_QUANTIDADE'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NR_QUANTIDADE ).
            WHEN 'NR_QUANTIDADE_2'.
              DS_VALOR = CONV #( WA_NOTA-NR_QUANTIDADE ).
            WHEN 'NR_VALOR'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NR_VALOR ).
            WHEN 'NR_VALOR_2'.
              DS_VALOR = CONV #( WA_NOTA-NR_VALOR ).
            WHEN 'DT_EMISSAO'.
              DS_VALOR = CONV #( WA_NOTA_ORG-DT_EMISSAO ).
            WHEN 'DT_EMISSAO_2'.
              DS_VALOR = CONV #( WA_NOTA-DT_EMISSAO ).
            WHEN 'DT_VENCIMENTO_FORM'.
              DS_VALOR = CONV #( WA_NOTA_ORG-DT_VENCIMENTO_FORM ).
            WHEN 'DT_VENCIMENTO_FORM_2'.
              DS_VALOR = CONV #( WA_NOTA-DT_VENCIMENTO_FORM ).
            WHEN 'CFOP'.
              DS_VALOR = CONV #( WA_NOTA_ORG-CFOP ).
            WHEN 'CFOP_2'.
              DS_VALOR = CONV #( WA_NOTA-CFOP ).
            WHEN 'NM_SERIE'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NM_SERIE ).
            WHEN 'NM_SERIE_2'.
              DS_VALOR = CONV #( WA_NOTA-NM_SERIE ).
            WHEN 'NR_CHAVE_NFE'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NR_CHAVE_NFE ).
            WHEN 'NR_CHAVE_NFE_2'.
              DS_VALOR = CONV #( WA_NOTA-NR_CHAVE_NFE ).
            WHEN 'DS_ENTREGUE_POR'.
              DS_VALOR = CONV #( WA_NOTA_ORG-DS_ENTREGUE_POR ).
            WHEN 'DS_ENTREGUE_POR_2'.
              DS_VALOR = CONV #( WA_NOTA-DS_ENTREGUE_POR ).
            WHEN 'NM_PESO_SUBTOTAL'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NM_PESO_SUBTOTAL ).
            WHEN 'NM_PESO_SUBTOTAL_2'.
              DS_VALOR = CONV #( WA_NOTA-NM_PESO_SUBTOTAL ).
          ENDCASE.

*% Umidade        GRID  DYNGSAMR  NR_PERC_UMI nrpercumi1  nrpercumi2
*% Impureza       GRID  DYNGSAMR  NR_PERC_IMP nrpercimp1  nrpercimp2
*% Avariado       GRID  DYNGSAMR  NR_PERC_AVA nrpercava1  nrpercava2
*% Ardido         GRID  DYNGSAMR  NR_PERC_ARD nrpercard1  nrpercard2
*% Quebrado       GRID  DYNGSAMR  NR_PERC_QUE nrpercque1  nrpercque2
*% Esverdeado     GRID  DYNGSAMR  NR_PERC_ESV nrpercesv1  nrpercesv2

          CASE WA_ENTITYS_ATRIBUTES_X_SAP-FIELDNAME.
            WHEN 'NR_PERC_UMI'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NR_PERC_UMI ).
            WHEN 'NR_PERC_UMI_2'.
              DS_VALOR = CONV #( WA_NOTA-NR_PERC_UMI ).
            WHEN 'NR_PERC_IMP'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NR_PERC_IMP ).
            WHEN 'NR_PERC_IMP_2'.
              DS_VALOR = CONV #( WA_NOTA-NR_PERC_IMP ).
            WHEN 'NR_PERC_AVA'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NR_PERC_AVA ).
            WHEN 'NR_PERC_AVA_2'.
              DS_VALOR = CONV #( WA_NOTA-NR_PERC_AVA ).
            WHEN 'NR_PERC_ARD'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NR_PERC_ARD ).
            WHEN 'NR_PERC_ARD_2'.
              DS_VALOR = CONV #( WA_NOTA-NR_PERC_ARD ).
            WHEN 'NR_PERC_QUE'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NR_PERC_QUE ).
            WHEN 'NR_PERC_QUE_2'.
              DS_VALOR = CONV #( WA_NOTA-NR_PERC_QUE ).
            WHEN 'NR_PERC_ESV'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NR_PERC_ESV ).
            WHEN 'NR_PERC_ESV_2'.
              DS_VALOR = CONV #( WA_NOTA-NR_PERC_ESV ).

            WHEN 'AL_NR_PERC_UMI'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_PERC_UMI ).
            WHEN 'AL_NR_PERC_IMP'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_PERC_IMP ).
            WHEN 'AL_NR_PERC_AVA'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_PERC_AVA ).
            WHEN 'AL_NR_PERC_ARD'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_PERC_ARD ).
            WHEN 'AL_NR_PERC_QUE'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_PERC_QUE ).
            WHEN 'AL_NR_PERC_ESV'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_PERC_ESV ).

          ENDCASE.

*Qtd.Umidade      GRID  DYNGSAMR  NR_QTDE_UMI nrqtdeumi1  nrqtdeumi2
*Qtd.Impureza     GRID  DYNGSAMR  NR_QTDE_IMP nrqtdeimp1  nrqtdeimp2
*Qtd.Avariado     GRID  DYNGSAMR  NR_QTDE_AVA nrqtdeava1  nrqtdeava2
*Qtd.Ardido       GRID  DYNGSAMR  NR_QTDE_ARD nrqtdeard1  nrqtdeard2
*Qtd.Quebrado     GRID  DYNGSAMR  NR_QTDE_QUE nrqtdeque1  nrqtdeque2
*Qtd.Esverdeado   GRID  DYNGSAMR  NR_QTDE_ESV nrqtdeesv1  nrqtdeesv2

          CASE WA_ENTITYS_ATRIBUTES_X_SAP-FIELDNAME.
            WHEN 'NR_QTDE_UMI'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NR_QTDE_UMI ).
            WHEN 'NR_QTDE_UMI_2'.
              DS_VALOR = CONV #( WA_NOTA-NR_QTDE_UMI ).
            WHEN 'NR_QTDE_IMP'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NR_QTDE_IMP ).
            WHEN 'NR_QTDE_IMP_2'.
              DS_VALOR = CONV #( WA_NOTA-NR_QTDE_IMP ).
            WHEN 'NR_QTDE_AVA'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NR_QTDE_AVA ).
            WHEN 'NR_QTDE_AVA_2'.
              DS_VALOR = CONV #( WA_NOTA-NR_QTDE_AVA ).
            WHEN 'NR_QTDE_ARD'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NR_QTDE_ARD ).
            WHEN 'NR_QTDE_ARD_2'.
              DS_VALOR = CONV #( WA_NOTA-NR_QTDE_ARD ).
            WHEN 'NR_QTDE_QUE'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NR_QTDE_QUE ).
            WHEN 'NR_QTDE_QUE_2'.
              DS_VALOR = CONV #( WA_NOTA-NR_QTDE_QUE ).
            WHEN 'NR_QTDE_ESV'.
              DS_VALOR = CONV #( WA_NOTA_ORG-NR_QTDE_ESV ).
            WHEN 'NR_QTDE_ESV_2'.
              DS_VALOR = CONV #( WA_NOTA-NR_QTDE_ESV ).

            WHEN 'AL_NR_QTDA_UMI'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_QTDA_UMI ).
            WHEN 'AL_NR_QTDA_IMP'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_QTDA_IMP ).
            WHEN 'AL_NR_QTDA_AVA'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_QTDA_AVA ).
            WHEN 'AL_NR_QTDA_ARD'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_QTDA_ARD ).
            WHEN 'AL_NR_QTDA_QUE'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_QTDA_QUE ).
            WHEN 'AL_NR_QTDA_ESV'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_QTDA_ESV ).

          ENDCASE.


        WHEN 'ZSDT0001CL'.

*Nome Outro Participante                                                  GRID  DYNGSAMR  DS_OUTRO_PARTIC      nomeoutropart1   nomeoutropart2
*Tipo deTransgeníase                                                      GRID  DYNGSAMR  TP_TRANSGENIA        tipodetransgen1  tipodetransgen2
*Tipo do Teste GMO Amaggi                                                 GRID  DYNGSAMR  IN_GMO               tipodotestegmo1  tipodotestegmo2
*Valor do Resultado RR1                                                   GRID  DYNGSAMR  NR_RESULTADO_01      valordoresrr1a   valordoresrr1b
*Valor do Resultado RR2                                                   GRID  DYNGSAMR  NR_RESULTADO_02      valordoresrr2a   valordoresrr2b
*Soma do RR1+RR2                                                          GRID  DYNGSAMR  NR_RES_RR1_RR2       somadorr1rr2a    somadorr1rr2b
*Tipo do Teste GMO - Tira Amaggi 0,3%. (Positivo/Negativo/Não Declarado)  GRID  DYNGSAMR  IN_GMO_03            ingmo03a         ingmo03b
*Informa se Existiu Outro Participante. (Sim/Não)                         GRID  DYNGSAMR  IN_SRR_ORIGEM_PARTIC insrrorigemp1    insrrorigemp2
*SRR Declarado (Sim/Não)                                                  GRID  DYNGSAMR  IN_SRR_DECLARADO     srrdeclarado1    srrdeclarado2
*Tipo do Teste GMO - Tira Monsanto 5%. (Positivo/Negativo/Não Declarado)  GRID  DYNGSAMR  IN_TESTE_SRR         intestesrr1      intestesrr2
*RR2 Declarado (Sim/Não)                                                  GRID  DYNGSAMR  IN_SRR_DECLARADO_2   rr2declarado1    rr2declarado2
*Tipo do Teste RR2 - Monsanto. (Positivo/Negativo/Não Declarado)          GRID  DYNGSAMR  IN_TESTE_SRR_2       intestesrr2a     intestesrr2b

          CASE WA_ENTITYS_ATRIBUTES_X_SAP-FIELDNAME.

            WHEN 'AL_ID_OUTRO_PARTIC'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_ID_OUTRO_PARTIC ).
            WHEN 'AL_NR_RESULTADO_01'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_RESULTADO_01 ).
            WHEN 'AL_NR_RESULTADO_02'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_RESULTADO_02 ).
            WHEN 'AL_NR_RES_RR1_RR2'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_NR_RES_RR1_RR2 ).
            WHEN 'AL_IN_GMO_03'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_IN_GMO_03 ).
            WHEN 'AL_IN_SRR_ORIGEM_PARTIC'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_IN_SRR_ORIGEM_PARTIC ).
            WHEN 'AL_IN_SRR_DECLARADO'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_IN_SRR_DECLARADO ).
            WHEN 'AL_IN_SRR_DECLARADO_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_IN_SRR_DECLARADO_2 ).
            WHEN 'AL_IN_TESTE_SRR_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_IN_TESTE_SRR_2 ).
            WHEN 'AL_TP_TRANSGENIA'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_TP_TRANSGENIA ).

            WHEN 'DS_OUTRO_PARTIC'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-DS_OUTRO_PARTIC ).
            WHEN 'DS_OUTRO_PARTIC_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-CARGA-DS_OUTRO_PARTIC ).
            WHEN 'TP_TRANSGENIA'.

              "Tipo deTransgeníase
              CASE E_APRESENTACAO_ORIGINAL-CARGA-TP_TRANSGENIA.
                WHEN ZIF_CARGA=>ST_TP_TRANSGENIASE_CO.
                  DS_VALOR = 'CO-Convencional'.
                WHEN ZIF_CARGA=>ST_TP_TRANSGENIASE_T1.
                  DS_VALOR = 'T1-RR Testado'.
                WHEN ZIF_CARGA=>ST_TP_TRANSGENIASE_T2.
                  DS_VALOR = 'T2-Teste Intacta Positivo'.
                WHEN ZIF_CARGA=>ST_TP_TRANSGENIASE_D1.
                  DS_VALOR = 'D1-Teste Intacta Negativo'.
                WHEN ZIF_CARGA=>ST_TP_TRANSGENIASE_D2.
                  DS_VALOR = 'D2-Declarado Intacta'.
                WHEN ZIF_CARGA=>ST_TP_TRANSGENIASE_PA.
                  DS_VALOR = 'PA-Participante'.
              ENDCASE.

            WHEN 'TP_TRANSGENIA_2'.

              CASE E_APRESENTACAO-CARGA-TP_TRANSGENIA.
                WHEN ZIF_CARGA=>ST_TP_TRANSGENIASE_CO.
                  DS_VALOR = 'CO-Convencional'.
                WHEN ZIF_CARGA=>ST_TP_TRANSGENIASE_T1.
                  DS_VALOR = 'T1-RR Testado'.
                WHEN ZIF_CARGA=>ST_TP_TRANSGENIASE_T2.
                  DS_VALOR = 'T2-Teste Intacta Positivo'.
                WHEN ZIF_CARGA=>ST_TP_TRANSGENIASE_D1.
                  DS_VALOR = 'D1-Teste Intacta Negativo'.
                WHEN ZIF_CARGA=>ST_TP_TRANSGENIASE_D2.
                  DS_VALOR = 'D2-Declarado Intacta'.
                WHEN ZIF_CARGA=>ST_TP_TRANSGENIASE_PA.
                  DS_VALOR = 'PA-Participante'.
              ENDCASE.

            WHEN 'AL_IN_GMO'.
              DS_VALOR = CONV #( E_APRESENTACAO-MANUTENCAO-AL_IN_GMO ).

            WHEN 'IN_GMO'.

              "Tipo do Teste GMO Amaggi
              CASE E_APRESENTACAO_ORIGINAL-CARGA-IN_GMO.
                WHEN ZIF_CARGA=>ST_GMO_NAO_TESTADO.
                  DS_VALOR = 'Não Testado'.
                WHEN ZIF_CARGA=>ST_GMO_NEGATIVO.
                  DS_VALOR = 'Negativo'.
                WHEN ZIF_CARGA=>ST_GMO_POSITIVO.
                  DS_VALOR = 'Positivo'.
              ENDCASE.

            WHEN 'IN_GMO_2'.

              CASE E_APRESENTACAO-CARGA-IN_GMO.
                WHEN ZIF_CARGA=>ST_GMO_NAO_TESTADO.
                  DS_VALOR = 'Não Testado'.
                WHEN ZIF_CARGA=>ST_GMO_NEGATIVO.
                  DS_VALOR = 'Negativo'.
                WHEN ZIF_CARGA=>ST_GMO_POSITIVO.
                  DS_VALOR = 'Positivo'.
              ENDCASE.

            WHEN 'NR_RESULTADO_01'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-NR_RESULTADO_01 ).

            WHEN 'NR_RESULTADO_01_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-CARGA-NR_RESULTADO_01 ).
            WHEN 'NR_RESULTADO_02'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-NR_RESULTADO_02 ).
            WHEN 'NR_RESULTADO_02_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-CARGA-NR_RESULTADO_02 ).
            WHEN 'NR_RES_RR1_RR2'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-NR_RES_RR1_RR2 ).
            WHEN 'NR_RES_RR1_RR2_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-CARGA-NR_RES_RR1_RR2 ).

            WHEN 'IN_GMO_03'.

              "Tipo do Teste GMO - Tira Amaggi 0,3%. (Positivo/Negativo/Não Declarado)
              CASE E_APRESENTACAO_ORIGINAL-CARGA-IN_GMO_03.
                WHEN ZIF_CARGA=>ST_GMO_NAO_TESTADO.
                  DS_VALOR = 'Não Testado'.
                WHEN ZIF_CARGA=>ST_GMO_NEGATIVO.
                  DS_VALOR = 'Negativo'.
                WHEN ZIF_CARGA=>ST_GMO_POSITIVO.
                  DS_VALOR = 'Positivo'.
              ENDCASE.

            WHEN 'IN_GMO_03_2'.

              CASE E_APRESENTACAO-CARGA-IN_GMO_03.
                WHEN ZIF_CARGA=>ST_GMO_NAO_TESTADO.
                  DS_VALOR = 'Não Testado'.
                WHEN ZIF_CARGA=>ST_GMO_NEGATIVO.
                  DS_VALOR = 'Negativo'.
                WHEN ZIF_CARGA=>ST_GMO_POSITIVO.
                  DS_VALOR = 'Positivo'.
              ENDCASE.

            WHEN 'IN_SRR_ORIGEM_PARTIC'.

              "Informa se Existiu Outro Participante. (Sim/Não)
              CASE E_APRESENTACAO_ORIGINAL-CARGA-IN_SRR_ORIGEM_PARTIC.
                WHEN ABAP_TRUE.
                  DS_VALOR = 'Sim'.
                WHEN ABAP_FALSE.
                  DS_VALOR = 'Não'.
              ENDCASE.

            WHEN 'IN_SRR_ORIGEM_PARTIC_2'.

              CASE E_APRESENTACAO-CARGA-IN_SRR_ORIGEM_PARTIC.
                WHEN ABAP_TRUE.
                  DS_VALOR = 'Sim'.
                WHEN ABAP_FALSE.
                  DS_VALOR = 'Não'.
              ENDCASE.

            WHEN 'IN_SRR_DECLARADO'.

              "SRR Declarado (Sim/Não)
              CASE E_APRESENTACAO_ORIGINAL-CARGA-IN_SRR_DECLARADO.
                WHEN ABAP_TRUE.
                  DS_VALOR = 'Sim'.
                WHEN ABAP_FALSE.
                  DS_VALOR = 'Não'.
              ENDCASE.

            WHEN 'IN_SRR_DECLARADO_2'.

              CASE E_APRESENTACAO-CARGA-IN_SRR_DECLARADO.
                WHEN ABAP_TRUE.
                  DS_VALOR = 'Sim'.
                WHEN ABAP_FALSE.
                  DS_VALOR = 'Não'.
              ENDCASE.

            WHEN 'IN_TESTE_SRR'.

              "Tipo do Teste GMO - Tira Monsanto 5%. (Positivo/Negativo/Não Declarado)
              CASE E_APRESENTACAO_ORIGINAL-CARGA-IN_TESTE_SRR.
                WHEN ZIF_CARGA=>ST_GMO_NAO_TESTADO.
                  DS_VALOR = 'Não Testado'.
                WHEN ZIF_CARGA=>ST_GMO_NEGATIVO.
                  DS_VALOR = 'Negativo'.
                WHEN ZIF_CARGA=>ST_GMO_POSITIVO.
                  DS_VALOR = 'Positivo'.
              ENDCASE.

            WHEN 'IN_TESTE_SRR_2'.

              CASE E_APRESENTACAO-CARGA-IN_TESTE_SRR.
                WHEN ZIF_CARGA=>ST_GMO_NAO_TESTADO.
                  DS_VALOR = 'Não Testado'.
                WHEN ZIF_CARGA=>ST_GMO_NEGATIVO.
                  DS_VALOR = 'Negativo'.
                WHEN ZIF_CARGA=>ST_GMO_POSITIVO.
                  DS_VALOR = 'Positivo'.
              ENDCASE.

            WHEN 'IN_SRR_DECLARADO_2A'.

              "RR2 Declarado (Sim/Não)
              CASE E_APRESENTACAO_ORIGINAL-CARGA-IN_SRR_DECLARADO_2.
                WHEN ABAP_TRUE.
                  DS_VALOR = 'Sim'.
                WHEN ABAP_FALSE.
                  DS_VALOR = 'Não'.
              ENDCASE.

            WHEN 'IN_SRR_DECLARADO_2B'.

              CASE E_APRESENTACAO-CARGA-IN_SRR_DECLARADO_2.
                WHEN ABAP_TRUE.
                  DS_VALOR = 'Sim'.
                WHEN ABAP_FALSE.
                  DS_VALOR = 'Não'.
              ENDCASE.

            WHEN 'IN_TESTE_SRR_2A'.

              "Tipo do Teste RR2 - Monsanto. (Positivo/Negativo/Não Declarado)
              CASE E_APRESENTACAO_ORIGINAL-CARGA-IN_TESTE_SRR_2.
                WHEN ZIF_CARGA=>ST_GMO_NAO_TESTADO.
                  DS_VALOR = 'Não Testado'.
                WHEN ZIF_CARGA=>ST_GMO_NEGATIVO.
                  DS_VALOR = 'Negativo'.
                WHEN ZIF_CARGA=>ST_GMO_POSITIVO.
                  DS_VALOR = 'Positivo'.
              ENDCASE.

            WHEN 'IN_TESTE_SRR_2B'.

              CASE E_APRESENTACAO-CARGA-IN_TESTE_SRR_2.
                WHEN ZIF_CARGA=>ST_GMO_NAO_TESTADO.
                  DS_VALOR = 'Não Testado'.
                WHEN ZIF_CARGA=>ST_GMO_NEGATIVO.
                  DS_VALOR = 'Negativo'.
                WHEN ZIF_CARGA=>ST_GMO_POSITIVO.
                  DS_VALOR = 'Positivo'.
              ENDCASE.

          ENDCASE.
      ENDCASE.

      DS_VALOR = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO =  DS_VALOR ).

      "Informações da Carga
      I_WORKFLOW->EDIT_ENTITY_NEWCHILD_VALUE_XML(
        EXPORTING
          I_ENTITYID = CONV #( WA_ENTITYS_ATRIBUTES_X_SAP-ENTITYID )
          I_TABELA   = CONV #( WA_ENTITYS_ATRIBUTES_X_SAP-TABNAME )
          I_CAMPO    = CONV #( WA_ENTITYS_ATRIBUTES_X_SAP-FIELDNAME )
          I_VALOR    = DS_VALOR
        CHANGING
          E_XML      = E_XML_ENTITY_CHILD ).

    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WS_INJECT~GET_ENTITYS_VALUES.

    DATA: DS_VALOR     TYPE STRING,
          CK_FISCAL    TYPE CHAR01,
*---> 20.06.2023 - Migração S4 - DG
"          CK_COMERCIAL TYPE CHAR02.
          CK_COMERCIAL TYPE ZCHAR02.
*<--- 20.06.2023 - Migração S4 - DG

    R_INSTANCE = ME.

    TRY .
        ME->AT_CARGA->GET_INFO_ALV_APRESENTACAO( IMPORTING E_APRESENTACAO = DATA(E_APRESENTACAO)
         )->GET_ROMANEIO_SAIDA(
              EXPORTING I_ID_CARGA  = ME->AT_CARGA->CARGA-ID_CARGA
              IMPORTING E_ROMANEIOS = DATA(E_ROMANEIOS_SAIDA)
         ).

        ME->AT_CARGA_ORIGINAL->GET_INFO_ALV_APRESENTACAO( IMPORTING E_APRESENTACAO = DATA(E_APRESENTACAO_ORIGINAL)
         )->GET_ROMANEIO_SAIDA(
              EXPORTING I_ID_CARGA  = ME->AT_CARGA_ORIGINAL->CARGA-ID_CARGA
              IMPORTING E_ROMANEIOS = DATA(E_ROMANEIOS_SAIDA_ORIGINAL)
         ).

      CATCH ZCX_ORDEM_CARREGAMENTO.    "
      CATCH ZCX_CARGA.    "
    ENDTRY.

    READ TABLE E_ROMANEIOS_SAIDA INDEX I_INDEX INTO DATA(WA_ROMANEIOS_SAIDA).
    READ TABLE E_APRESENTACAO-ORDEM_VENDA INDEX I_INDEX INTO DATA(WA_ORDEM_VENDA).
    READ TABLE E_APRESENTACAO-PEDIDO_COMPRA INDEX I_INDEX INTO DATA(WA_PEDIDO_COMPRA).

    READ TABLE E_ROMANEIOS_SAIDA_ORIGINAL INDEX I_INDEX INTO DATA(WA_ROMANEIOS_SAIDA_ORG).
    READ TABLE E_APRESENTACAO_ORIGINAL-ORDEM_VENDA INDEX I_INDEX INTO DATA(WA_ORDEM_VENDA_ORG).
    READ TABLE E_APRESENTACAO_ORIGINAL-PEDIDO_COMPRA INDEX I_INDEX INTO DATA(WA_PEDIDO_COMPRA_ORG).

    E_RELATIONSHIP = ME->AT_CARGA->CARGA-ID_CARGA.

    LOOP AT I_WORKFLOW->AT_ENTITYS_ATRIBUTES INTO DATA(WA_ENTITYS_ATRIBUTES_X_SAP)
        WHERE ENTITYID EQ I_ENTITYID.

      CASE WA_ENTITYS_ATRIBUTES_X_SAP-TABNAME.
        WHEN 'ZSDT0001ACB'.

          CASE WA_ENTITYS_ATRIBUTES_X_SAP-FIELDNAME.
            WHEN 'CK_ACEITE_FILIAL'.
              CASE E_APRESENTACAO-MANUTENCAO-CK_ACEITE_FILIAL.
                WHEN ABAP_TRUE.
                  DS_VALOR = '1'.
                WHEN ABAP_FALSE.
                  DS_VALOR = '0'.
              ENDCASE.
            WHEN 'CK_ACEITE_FISCAL'.
              CASE E_APRESENTACAO-MANUTENCAO-CK_ACEITE_FISCAL.
                WHEN ABAP_TRUE.
                  DS_VALOR = '1'.
                WHEN ABAP_FALSE.
                  DS_VALOR = '0'.
              ENDCASE.
            WHEN 'CK_ACEITE_COMERCIAL'.
              CASE E_APRESENTACAO-MANUTENCAO-CK_ACEITE_COMERCIAL.
                WHEN ABAP_TRUE.
                  DS_VALOR = '1'.
                WHEN ABAP_FALSE.
                  DS_VALOR = '0'.
              ENDCASE.
          ENDCASE.

        WHEN 'ZSDT0001CG' OR 'ZSDT0001OV' OR 'ZSDT0001EK'.

*Número do Romaneio de Saída            FORM  DYNSAMR NR_ROMANEIO_SAI romansaida
*Nº documento de vendas e distribuição  FORM  DYNSAMR NR_ORDEM_VENDA  nrordemvenda
*Data de Movimento                      FORM  DYNSAMR DT_MOVIMENTO    dtmovimento
*Filial de Recebimento                  FORM  DYNSAMR ID_BRANCH       codigofilial
*Filial de Recebimento                  FORM  DYNSAMR NAME            nmfilial
*Local de Coleta                        FORM  DYNSAMR ID_LOCAL_COLETA idlocaldecol
*Nome Local de Coleta                   FORM  DYNSAMR DS_LOCAL_COLETA nomelocaldec
*Incoterms                              FORM  DYNSAMR TP_FRETE        tpfrete


          "Informações da Orden de Venda / Romaneio de Saída
          CASE WA_ENTITYS_ATRIBUTES_X_SAP-FIELDNAME.
            WHEN 'NR_ROMANEIO_SAI'.
              DS_VALOR = CONV #( WA_ROMANEIOS_SAIDA-NR_ROMANEIO ).
            WHEN 'NR_ORDEM_VENDA'.
              IF WA_ORDEM_VENDA-NR_ORDEM_VENDA IS NOT INITIAL.
                DS_VALOR = CONV #( WA_ORDEM_VENDA-NR_ORDEM_VENDA ).
              ELSE.
                DS_VALOR = CONV #( WA_PEDIDO_COMPRA-NR_PEDIDO_COMPRA ).
              ENDIF.
            WHEN 'DT_MOVIMENTO'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-DT_MOVIMENTO ).
            WHEN 'ID_BRANCH'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-ID_BRANCH ).
            WHEN 'NAME'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-NAME ).
            WHEN 'ID_LOCAL_COLETA'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-ID_LOCAL_COLETA ).
            WHEN 'DS_LOCAL_COLETA'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-DS_LOCAL_COLETA ).
            WHEN 'TP_FRETE'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-TP_FRETE ).
          ENDCASE.

*Placa Veículo Tranção            FROM  DYNSAMR DS_PLACA_TRATOR   dsplacatrator1  dsplacatrator2
*Nome do Proprietário do Veículo  FROM  DYNSAMR DS_PROPRIETARIO   dsproprietario1 dsproprietario2
*Placa Veículo Reboque 1          FROM  DYNSAMR DS_PLACA_REBOQ_1  dsplacareboq1a  dsplacareboq1b
*Placa Veículo Reboque 2          FROM  DYNSAMR DS_PLACA_REBOQ_2  dsplacareboq2a  dsplacareboq2b
*Placa Veículo Reboque 3          FROM  DYNSAMR DS_PLACA_REBOQ_3  dsplacareboq3a  dsplacareboq3b
*Nome do Motorista                FROM  DYNSAMR DS_MOTORISTA      dsmotorista1    dsmotorista2
*Número do Ticket                 FROM  DYNSAMR NR_TICKET         nmerodoticke1   nmerodoticke2
*Classificadora                   FROM  DYNSAMR DS_CLASSIFICADORA dsclassificado1 dsclassificado2
*Local de entrega                 FROM  DYNSAMR DS_LOCAL_ENTREGA  localdeentreg1  localdeentreg2
*Peso Bruto do Caminhão           FROM  DYNSAMR NM_PESO_BRUTO     pesobrutodoc1   pesobrutodoc2
*Peso Tara do Caminhão            FROM  DYNSAMR NM_PESO_TARA      pesotaradoca1   pesotaradoca2
*Peso SubTotal do Caminhão        FROM  DYNSAMR NM_PESO_SUBTOTAL  pesosubtotald1  pesosubtotald2

          CASE WA_ENTITYS_ATRIBUTES_X_SAP-FIELDNAME.
            WHEN 'AL_DS_PLACA_TRATOR'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-MANUTENCAO-AL_DS_PLACA_TRATOR ).
            WHEN 'AL_ID_PROPRIETARIO'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-MANUTENCAO-AL_ID_PROPRIETARIO ).
            WHEN 'AL_DS_PLACA_REBOQ_1'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-MANUTENCAO-AL_DS_PLACA_REBOQ_1 ).
            WHEN 'AL_DS_PLACA_REBOQ_2'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-MANUTENCAO-AL_DS_PLACA_REBOQ_2 ).
            WHEN 'AL_DS_PLACA_REBOQ_2'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-MANUTENCAO-AL_DS_PLACA_REBOQ_3 ).
            WHEN 'AL_ID_MOTORISTA'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-MANUTENCAO-AL_ID_MOTORISTA ).
            WHEN 'AL_NR_TICKET'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-MANUTENCAO-AL_NR_TICKET ).
            WHEN 'AL_ID_CLASSIFICADORA'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-MANUTENCAO-AL_ID_CLASSIFICADORA ).
            WHEN 'AL_ID_LOCAL_ENTREGA'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-MANUTENCAO-AL_ID_LOCAL_ENTREGA ).
            WHEN 'AL_NM_PESO_BRUTO'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-MANUTENCAO-AL_NM_PESO_BRUTO ).
            WHEN 'AL_NM_PESO_TARA'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-MANUTENCAO-AL_NM_PESO_TARA ).
            WHEN 'AL_NM_PESO_SUBTOTAL'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-MANUTENCAO-AL_NM_PESO_SUBTOTAL ).

            WHEN 'ID_PRODUTO'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-ID_PRODUTO ).
            WHEN 'DS_PRODUTO'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-DS_PRODUTO ).
            WHEN 'ID_AGENT_FRETE'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-ID_AGENT_FRETE ).
            WHEN 'DS_AGENT_FRETE'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-DS_AGENT_FRETE ).
            WHEN 'ID_LOCAL_DESTINO'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-ID_LOCAL_DESTINO ).
            WHEN 'DS_LOCAL_DESTINO'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-DS_LOCAL_DESTINO ).
            WHEN 'ID_LOCAL_DESCARGA'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-ID_LOCAL_DESCARGA ).
            WHEN 'DS_LOCAL_DESCARGA'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-DS_LOCAL_DESCARGA ).

            WHEN 'DS_PLACA_TRATOR'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-DS_PLACA_TRATOR ).
            WHEN 'DS_PLACA_TRATOR_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-CARGA-DS_PLACA_TRATOR ).
            WHEN 'DS_PROPRIETARIO'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-DS_PROPRIETARIO ).
            WHEN 'DS_PROPRIETARIO_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-CARGA-DS_PROPRIETARIO ).
            WHEN 'DS_PLACA_REBOQ_1'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-DS_PLACA_REBOQ_1 ).
            WHEN 'DS_PLACA_REBOQ_1_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-CARGA-DS_PLACA_REBOQ_1 ).
            WHEN 'DS_PLACA_REBOQ_2'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-DS_PLACA_REBOQ_2 ).
            WHEN 'DS_PLACA_REBOQ_2_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-CARGA-DS_PLACA_REBOQ_2 ).
            WHEN 'DS_PLACA_REBOQ_3'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-DS_PLACA_REBOQ_3 ).
            WHEN 'DS_PLACA_REBOQ_3_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-CARGA-DS_PLACA_REBOQ_3 ).
            WHEN 'DS_MOTORISTA'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-DS_MOTORISTA ).
            WHEN 'DS_MOTORISTA_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-CARGA-DS_MOTORISTA ).
            WHEN 'NR_TICKET'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-NR_TICKET ).
            WHEN 'NR_TICKET_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-CARGA-NR_TICKET ).
            WHEN 'DS_CLASSIFICADORA'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-DS_CLASSIFICADORA ).
            WHEN 'DS_CLASSIFICADORA_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-CARGA-DS_CLASSIFICADORA ).
            WHEN 'DS_LOCAL_ENTREGA'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-DS_LOCAL_ENTREGA ).
            WHEN 'DS_LOCAL_ENTREGA_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-CARGA-DS_LOCAL_ENTREGA ).
            WHEN 'NM_PESO_BRUTO'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-NM_PESO_BRUTO ).
            WHEN 'NM_PESO_BRUTO_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-CARGA-NM_PESO_BRUTO ).
            WHEN 'NM_PESO_TARA'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-NM_PESO_TARA ).
            WHEN 'NM_PESO_TARA_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-CARGA-NM_PESO_TARA ).
            WHEN 'NM_PESO_SUBTOTAL'.
              DS_VALOR = CONV #( E_APRESENTACAO_ORIGINAL-CARGA-NM_PESO_SUBTOTAL ).
            WHEN 'NM_PESO_SUBTOTAL_2'.
              DS_VALOR = CONV #( E_APRESENTACAO-CARGA-NM_PESO_SUBTOTAL ).
          ENDCASE.

      ENDCASE.

      DS_VALOR = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = DS_VALOR ).

      "Informações da Carga
      I_WORKFLOW->EDIT_ENTITY_VALUE_XML(
        EXPORTING
          I_ENTITYID = CONV #( WA_ENTITYS_ATRIBUTES_X_SAP-ENTITYID )
          I_TABELA   = CONV #( WA_ENTITYS_ATRIBUTES_X_SAP-TABNAME )
          I_CAMPO    = CONV #( WA_ENTITYS_ATRIBUTES_X_SAP-FIELDNAME )
          I_VALOR    = DS_VALOR
        CHANGING
          E_XML      = E_XML_ENTITY ).

    ENDLOOP.

    IF ME->OBS_ALTERACAO IS NOT INITIAL AND I_ENTITYID EQ 'SAMR'.
      I_WORKFLOW->EDIT_ENTITY_VALUE_XML(
        EXPORTING
          I_ENTITYID               = I_ENTITYID
          I_TABELA                 = 'ZSDT0001CG'
          I_CAMPO                  = 'OBS_ALTERACAO'
          I_VALOR                  = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = CONV #( ME->OBS_ALTERACAO ) )
          I_ATTRIBUTEID            = 'ALTREAL'
          I_TYPE                   = ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_TEXTO
        CHANGING
          E_XML                    = E_XML_ENTITY ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WS_INJECT~GET_INSTANCE.

    IF ZIF_SOFT_EXPERT_WS_INJECT~AT_INJECT IS NOT BOUND.
      CREATE OBJECT ZIF_SOFT_EXPERT_WS_INJECT~AT_INJECT TYPE ZCL_SE_WL_MANUTENCAO_ROMANEIO.
      ZIF_SOFT_EXPERT_WS_INJECT~AT_INJECT->I_ID_PROCESSID =
      ZCL_SOFT_EXPERT_WORKFLOW=>ZIF_SOFT_EXPERT_WORKFLOW~GET_PROCESSID_SE( I_PROCESSID_SAP = ZIF_SOFT_EXPERT_WORKFLOW=>ST_PROCESS_MANUTENCAO_ROMANEIO ).
    ENDIF.

    R_INSTANCE = ZIF_SOFT_EXPERT_WS_INJECT~AT_INJECT.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WS_INJECT~GET_MANY_EDITENTITYS_RECORDS.

    R_INSTANCE = ME.
    READ TABLE I_WORKFLOW->AT_ENTITYS_ATRIBUTES WITH KEY ENTITYID = I_ENTITYID INTO DATA(WA_ENTITYS_ATRIBUTES_X_SAP).

    TRY .
        ME->AT_CARGA->GET_INFO_ALV_APRESENTACAO( IMPORTING E_APRESENTACAO = DATA(E_APRESENTACAO) ).
      CATCH ZCX_ORDEM_CARREGAMENTO.    "
      CATCH ZCX_CARGA.    "
    ENDTRY.

    CASE WA_ENTITYS_ATRIBUTES_X_SAP-TABNAME.
      WHEN 'ZSDT0001CG' OR 'ZSDT0001OV' OR 'ZSDT0001EK'.
        E_MANY = 1.
      WHEN 'ZSDT0001NT' OR 'ZSDT0001CL'.
        DESCRIBE TABLE E_APRESENTACAO-NOTAS LINES E_MANY.
      WHEN OTHERS.
        "Raise
    ENDCASE.

  ENDMETHOD.


  method ZIF_SOFT_EXPERT_WS_INJECT~GET_RELATIONSHIPS_VALUES.
    CLEAR: E_RELATIONSHIP.
    R_INSTANCE = ME.
  endmethod.


  METHOD ZIF_SOFT_EXPERT_WS_INJECT~GET_WORKFLOW_TITLE.

    E_WORKFLOW_TITLE = ''.
    R_INSTANCE = ME.

  ENDMETHOD.
ENDCLASS.
