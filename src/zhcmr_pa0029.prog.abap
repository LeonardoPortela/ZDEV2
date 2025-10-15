*&---------------------------------------------------------------------*
*& Relatório - Comprovante de Pagamento
*&---------------------------------------------------------------------*
*& Developer.: Enio Jesus
*& Data......: 05/10/2017
*&---------------------------------------------------------------------*

REPORT  zhcmr_pa0029.
TYPE-POOLS: slis, vrm.

TABLES: zhcmt_pa_0009, somlreci1, bseg.

TYPES: BEGIN OF ty_saida,
         check          TYPE c,
         bukrs          TYPE zhcmt_pa_0009-bukrs,
         cnpj_empresa   TYPE zhcmt_pa_0009-cnpj_empresa,
         agencia        TYPE char10,
         conta_corrente TYPE char15,
         banco          TYPE char3,
         vblnr          TYPE zhcmt_pa_0009-vblnr,
         dt_pgto        TYPE sy-datum,
         vlr_pgto       TYPE zhcmt_pa_0009-vlr_pgto,
         matricula      TYPE zhcmt_pa_0009-pernr,
         nome           TYPE lfa1-name1,
         cpf            TYPE zhcmt_pa_0009-cpf,
         bco_fav        TYPE zhcmt_pa_0009-bco_fav,
         ag_fav         TYPE char10,
         cc_fav         TYPE char15,
         cod_autent     TYPE zhcmt_pa_0009-cod_autent,
         motivo(50)     TYPE c,
         desc_mot(500)  TYPE c,
       END OF ty_saida,

       BEGIN OF ty_msg_error,
         texto_breve TYPE crmt_bl_description,
       END OF ty_msg_error.

TYPES: BEGIN OF ty_motivos ,
         codigo(2)      TYPE c,
         descricao(130) TYPE c,
       END OF ty_motivos.

DATA: comprovantes     TYPE TABLE OF zhcmt_pa_0009,
      comprovantes_aux TYPE TABLE OF zhcmt_pa_0009,
      wa_comprovantes  TYPE zhcmt_pa_0009,
      gt_msg_error     TYPE TABLE OF ty_msg_error,
      gt_fcat_slis     TYPE slis_t_fieldcat_alv,
      gt_t001          TYPE TABLE OF t001,
      gt_saida         TYPE TABLE OF ty_saida,
      gt_docs          TYPE STANDARD TABLE OF docs,
      gw_tline         TYPE TABLE OF tline WITH HEADER LINE,
      gw_record        LIKE solisti1 OCCURS 0 WITH HEADER LINE,
      wl_saida         TYPE ty_saida,
*      WL_ZFIT0091     TYPE ZFIT0091,
      wl_layout        TYPE slis_layout_alv,
      wl_variant       TYPE disvariant,
      wl_output_opt    TYPE ssfcompop,
      wl_document_opt  TYPE ssfcrespd,
      wl_control       TYPE ssfctrlop,
      wl_job_output    TYPE ssfcrescl,
      wl_t001          TYPE t001,
      name             TYPE vrm_id,
      list             TYPE vrm_values,
      value            LIKE LINE OF list,
      v_func_name      TYPE rs38l_fnam,
      v_return         TYPE sy-subrc,
      v_empresas       TYPE char100,
      t_motivos        TYPE TABLE OF ty_motivos,
      wa_motivos       TYPE ty_motivos,

      t_motivo_smart   TYPE TABLE OF ty_motivos,
      wa_motivo_smart  TYPE ty_motivos,

      t_saida_msg      TYPE catsxt_longtext_itab,
      wa_saida_msg     TYPE LINE OF catsxt_longtext_itab.


CONSTANTS:
  c_gerar_comprovante  TYPE char4    VALUE '%GC',
  c_enviar_email       TYPE char4    VALUE '%EM',
  c_smart_forms        TYPE tdsfname VALUE 'ZHCMS_PA0016',
  c_smart_forms_recusa TYPE tdsfname VALUE 'ZHCMS_PA0017'.


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_bukrs  FOR zhcmt_pa_0009-bukrs NO INTERVALS OBLIGATORY NO-EXTENSION,
                s_werks  FOR bseg-gsber OBLIGATORY,
                s_pernr  FOR zhcmt_pa_0009-pernr,
                s_dt_pg  FOR zhcmt_pa_0009-dt_pgto.
SELECTION-SCREEN SKIP 1.

PARAMETERS p_banco AS LISTBOX VISIBLE LENGTH 22 DEFAULT '1' OBLIGATORY.
SELECTION-SCREEN SKIP 1.
PARAMETERS p_recusa AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF SCREEN 0110 AS SUBSCREEN.
SELECT-OPTIONS: s_email FOR somlreci1-receiver NO INTERVALS.
SELECTION-SCREEN END OF SCREEN 0110.


AT SELECTION-SCREEN OUTPUT.

  CLEAR: value.

  name       = 'P_BANCO'.

  value-key  = '1'.
  value-text = '001 - BANCO DO BRASIL'.
  APPEND value TO list.

  value-key  = '2'.
  value-text = '237 - BANCO BRADESCO'.
  APPEND value TO list.

  value-key  = '3'.
  value-text = '399 - BANCO HSBC'.
  APPEND value TO list.

  value-key  = '4'.
  value-text = '033 - BANCO SANTANDER'.
  APPEND value TO list.

*  VALUE-KEY  = '4'.
*  VALUE-TEXT = '341 - BANCO ITAÚ'.
*  APPEND VALUE TO LIST.
*
*  VALUE-KEY  = '5'.
*  VALUE-TEXT = '748 - SICREDI'.
*  APPEND VALUE TO LIST.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = name
      values          = list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.


START-OF-SELECTION.
***///CS2021000461 Acesso com permissões irrestritas - Confidencial FOPAG - Set/2021 - Inicio
***
***  "// Autorização empresa
***  PERFORM CHECK_AUTHORIZATION USING
***                              'ZHR_PG_FOL'
***                              S_BUKRS-LOW
***                              'BUKRS'
***                              CHANGING
***                              V_RETURN.
***
***  IF ( V_RETURN IS NOT INITIAL ).
***    MESSAGE S836(SD) WITH TEXT-006 S_BUKRS-LOW '.' DISPLAY LIKE 'E'.
***    EXIT.
***  ENDIF.
***
  "// Autorização centro
  LOOP AT s_werks.
***///CS2021000461 Acesso com permissões irrestritas - Confidencial FOPAG - Set/2021 - Inicio
***    PERFORM CHECK_AUTHORIZATION USING
***                                'ZHR_PG_FOL'
***                                S_WERKS-LOW
***                                'WERKS'
***                                CHANGING
***                                V_RETURN.

    PERFORM check_authorization USING
                             'P_ORGIN'
                              s_werks-low
                             'PERSA'
                             CHANGING
                             v_return.
***///CS2021000461 Acesso com permissões irrestritas - Confidencial FOPAG - Set/2021 - Fim
    IF ( v_return IS NOT INITIAL ).
      MESSAGE s836(sd) WITH text-007 s_werks-low '.' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDLOOP.

  CHECK ( v_return IS INITIAL ).

  PERFORM seleciona_dados.

END-OF-SELECTION.


FORM z_hotspot_report USING p_i_selfield TYPE  slis_selfield.

  CASE p_i_selfield-fieldname.

    WHEN: 'DESC_MOT'.
      READ TABLE gt_saida[] INTO wl_saida INDEX p_i_selfield-tabindex.
      PERFORM mensagem USING wl_saida-motivo.

  ENDCASE.

ENDFORM.


FORM check_authorization USING
                         p_object
                         p_field
                         p_id
                         CHANGING
                         return.

  AUTHORITY-CHECK OBJECT p_object ID p_id
  FIELD p_field.

  return = sy-subrc.
ENDFORM.

FORM preenche_tab_mot.

  IF wa_comprovantes-archive+0(9) = 'PAG237_10'.

    PERFORM motivo USING:
          'AA'    ' Arquivo Dupliaco',
          'AB'    ' Data limite para desconto, sem valor correspondente',
          'AC'    ' Tipo de serviço inválido',
          'AD'    ' Modalidade de pagamento inválida',
          'AE'    ' Tipo de inscrição e identificação do cliente pagador incompatíveis',
          'AF'    ' Valores não numéricos ou zerados',
          'AG'    ' Tipo de inscrição e identificação do favorecido incompatível',
          'AJ'    ' Tipo de movimento inválido',
          'AL'    ' Banco, agência ou conta inválido',
          'AM'    ' Agência do favorecido inválida',
          'AN'    ' Conta corrente do favorecido inválida',
          'AO'    ' Nome do favorecido não informado',
          'AQ'    ' Tipo de moeda inválido',
          'AT'    ' CGC/CPF do favorecido inválido',
          'AU'    ' Endereço do favorecido não informado',
          'AX'    ' CEP do favorecido inválido',
          'AY'    ' Alteração inválida; Banco anterior Bradesco',
          'AZ'    ' Código de Banco do favorecido inválido',
          'BE'    ' Hora de gravação inválida',
          'BF'    ' Identificação da empresa no Banco, inválida',
          'BG'    ' CGC/CPF do pagador inválido',
          'BH'    ' Tipo de inscrição do cliente favorecido inválido',
          'BI'    ' Data de vencimento inválida ou não preenchida',
          'BJ'    ' Data de emissão do documento inválida',
          'BK'    ' Tipo de inscrição do cliente favorecido não permitido',
          'BL'    ' Data limite para desconto inválida',
          'BM'    ' Data para efetivação do pagamento inválida',
          'BN'    ' Data para efetivação anterior a do processamento',
          'BO'    ' Cliente não cadastrado',
          'BP'    ' Identificação de Título Bradesco divergente da original',
          'BQ'    ' Data do documento posterior ao vencimento',
          'BT'    ' Desautorização efetuada',
          'FA'    ' Código de origem inválido',
          'FB'    ' Data de gravação do arquivo inválida',
          'FC'    ' Tipo de documento inválido',
          'FE'    ' Número de pagamento inválido',
          'FF'    ' Valor do desconto sem data limite',
          'FG'    ' Data limite para desconto posterior ao vencimento',
          'FH'    ' Falta número e/ou série do documento',
          'FI'    ' Exclusão de agendamento não disponível',
          'FJ'    ' Soma dos valores não confere',
          'FK'    ' Falta valor de pagamento',
          'FL'    ' Modalidade de pagamento inválida para o contrato',
          'FM'    ' Código de movimento inválido',
          'FN'    ' Tentativa de inclusão de registro existente',
          'FO'    ' Tentativa de alteração para registro inexistente ',
          'FP'    ' Tentativa de efetivação de agendamento não disponível',
          'FQ'    ' Tentativa de desautorização de agendamento não disponível',
          'FR'    ' Autorização de agendamento sem data de efetivação e sem data de  vencimento',
          'FT'    ' Tipo de inscrição do cliente pagador inválido',
          'FU'    ' Contrato inexistente ou inativo',
          'FV'    ' Cliente com convênio cancelado',
          'FW'    ' Valor autorizado inferior ao original ',
          'FX'    ' Está faltando registro header',
          'FZ'    ' Valor autorizado não confere para pagamento em atraso',
          'F0'    ' Agendamento em atraso; não permitido pelo convênio',
          'F1'    ' Tentativa de Agendamento com Desc. Fora do Prazo',
          'F3'    ' Tentativa de alteração inválida; confirmação de débito já efetuada',
          'F4'    ' Falta registro trailler',
          'F5'    ' Valor do trailler não confere',
          'F6'    ' Quantidade de registros do trailler não confere',
          'F7'    ' Tentativa de alteração inválida; pagamento já enviado ao Bradesco Instantâneo',
          'F8'    ' Pagamento enviado após o horário estipulado',
          'F9'    ' Tentativa de inclusão de registro existente em histórico',
          'GA'    ' Tipo de DOC/TED inválido',
          'GB'    ' Número do DOC/TED inválido',
          'GC'    ' Finalidade do DOC/TED inválida ou inexistente',
          'GD'    ' Conta corrente do favorecido encerrada / bloqueada',
          'GE'    ' Conta corrente do favorecido não recadastrada',
          'GF'    ' Inclusão de pagamento via modalidade 30 não permitida',
          'GG'    ' Campo livre do código de barras (linha digitável) inválido',
          'GH'    ' Dígito verificador do código de barras inválido',
          'GI'    ' Código da moeda da linha digitável inválido',
          'GJ'    ' Conta poupança do favorecido inválida',
          'GK'    ' Conta poupança do favorecido não recadastrada',
          'GL'    ' Conta poupança do favorecido não encontrada',
          'GM'    ' Pagamento 3 (três) dias após o vencimento',
          'GN'    ' Conta complementar inválida',
          'GO'    ' Inclusão de DOC/TED para Banco 237 não permitido ',
          'GP'    ' CGC/CPF do favorecido divergente do cadastro do Banco',
          'GQ'    ' Tipo de DOC/TED não permitido via sistema eletrônico',
          'GR'    ' Alteração inválida; pagamento já enviado a agência pagadora',
          'GS'    ' Limite de pagamento excedido. Fale com o Gerente da sua agência',
          'GT'    ' Limite vencido/vencer em 30 dias',
          'GU'    ' Pagamento agendado por aumento de limite ou redução no total autorizado',
          'GV'    ' Cheque OP estornado conforme seu pedido',
          'GW'    ' Conta corrente ou conta poupança com razão não permitido para efetivação de crédito',
          'GX'    ' Cheque OP com data limite vencida',
          'GY'    ' Conta poupança do favorecido encerrada / bloqueada',
          'GZ'    ' Conta corrente do pagador encerrada / bloqueada',
          'HB'    ' Pagamento não efetuado, saldo insuficiente',
          'HC'    ' Pagamento não efetuado, além de saldo insuficiente, conta com  cadastro no DVL',
          'HD'    ' Pagamento não efetuado, além de saldo insuficiente, conta bloqueada',
          'HE'   	' Data de Vencto/Pagto fora do prazo de operação do banco',
          'HG'    ' Processado e não debitado por saldo insuficiente',
          'HI'    ' Cheque OP Emitido nesta data ',
          'JA'    ' Código de lançamento inválido',
          'JB'    ' DOC/TED/Títulos devolvidos e estornados',
          'JC'    ' Modalidade alterada de 07/CIP, para 08/STR',
          'JD'    ' Modalidade alterada de 07/CIP, para 03/DOC COMPE',
          'JE'    ' Modalidade alterada de 08/STR para 07/CIP',
          'JF'    ' Modalidade alterada de 08/STR para 03/COMPE',
          'JG'    ' Alteração de Modalidade Via Arquivo não Permitido',
          'JH'    ' Horário de Consulta de Saldo após Encerramento Rotina',
          'JI'    ' Modalidade alterada de 01/Crédito em conta para 05/Crédito em conta real time',
          'JJ'    ' Horário de agendamento Inválido',
          'JK'    ' Tipo de conta – modalidade DOC/TED - inválido',
          'JM'    ' Alteração não Permitida, Titulo Antecipado/Descontado',
          'JN'    ' Modalidade Alter. de 05/Crédito em Conta Real Time Para 01/Crédito em conta',
          'JO'    ' Exclusão não Permitida Titulo Antecipado/Descontado',
          'JP'    ' Pagamento com Limite TED Excedido. Fale com o Gerente da sua agência para Autorização',
          'KO'    ' Autorização para debito em conta',
          'KP'    ' Cliente pagador não cadastrado do PAGFOR',
          'KQ'    ' Modalidade inválida para pagador em teste',
          'KR'    ' Banco destinatário não operante nesta data',
          'KS'    ' Modalidade alterada de DOC. Para TED',
          'KT'    ' Dt. Efetivação alterada p/ próximo MOVTO. ** TRAG',
          'KV'    ' CPF/CNPJ do investidor inválido ou inexistente',
          'KW'    ' Tipo Inscrição Investidor Inválido ou inexistente',
          'KX'    ' Nome do Investidor Inexistente',
          'KZ'    ' Código do Investidor Inexistente',
          'LB'    ' Pagamento não autorizado sob Lista de Débito',
          'LC'    ' Lista com mais de uma modalidade ',
          'LD'    ' Lista com mais de uma data de Pagamento',
          'LE'    ' Número de Lista Duplicado',
          'LF'    ' Lista de Débito vencida e não autorizada',
          'LG'    ' Conta Salário não permitida para este convênio',
          'LH'    ' Código de Lançamento inválido para Conta Salário',
          'LI'    ' Finalidade de DOC / TED inválido para Salário',
          'LJ'    ' Conta Salário obrigatória para este Código de Lançamento',
          'LK'    ' Tipo de Conta do Favorecido Inválida',
          'LL'    ' Nome do Favorecido Inconsistente',
          'LM'    ' Número de Lista de Débito Inválido',
          'MA'    ' Tipo conta Inválida para finalidade',
          'MB'    ' Conta Crédito Investimento inválida/inexistente',
          'MC'    ' Conta Débito Investimento Inválida/inexistente',
          'MD'    ' Titularidade diferente para tipo de conta',
          'ME'    ' Data de Pagamento Alterada devido a Feriado Local',
          'MF'    ' Alegação Efetuada',
          'MG'    ' Alegação Não Efetuada. Motivo da Alegação/Reconhecimento da Divida Inconsistente',
          'MH'    ' Autorização Não Efetuada. Código de Reconhecimento da divida não permitido',
          'NC'    ' Código Identificador Inválid',
          'TR'    ' Ag/ Conta do favorecido alterado por Transferência de agencia.'.
  ELSE.
    PERFORM motivo USING:
          '01'    ' Insuficiência de Fundos - Débito Não Efetuado',
          '02'    ' Crédito ou Débito Cancelado pelo Pagador/Credor',
          'AA'    ' Controle Inválido',
          'AB'    ' Tipo de Operação Inválido',
          'AC'    ' Tipo de Serviço Inválido',
          'AD'    ' Forma de Lançamento Inválida',
          'AE'    ' Tipo/Número de Inscrição Inválido',
          'AF'    ' Código de Convênio Inválido',
          'AG'    ' Agência/Conta Corrente/DV Inválido',
          'AH'    ' Nº Sequencial do Registro no Lote Inválido',
          'AI'    ' Código de Segmento de Detalhe Inválido',
          'AJ'    ' Tipo de Movimento Inválido',
          'AK'    ' Código da Câmara de Compensação do Banco Favorecido/Depositário Inválido',
          'AL'    ' Código do Banco Favorecido ou Depositário Inválido',
          'AM'    ' Agência Mantenedora da Conta Corrente do Favorecido Inválida',
          'AN'    ' Conta Corrente/DV do Favorecido Inválido',
          'AO'    ' Nome do Favorecido Não Informado',
          'AP'    ' Data Lançamento Inválido',
          'AQ'    ' Tipo/Quantidade da Moeda Inválido',
          'AR'    ' Valor do Lançamento Inválido',
          'AS'    ' Aviso ao Favorecido - Identificação Inválida',
          'AT'    ' Tipo/Número de Inscrição do Favorecido Inválido',
          'AU'    ' Logradouro do Favorecido Não Informado',
          'AV'    ' Nº do Local do Favorecido Não Informado',
          'AW'    ' Cidade do Favorecido Não Informada',
          'AX'    ' CEP/Complemento do Favorecido Inválido',
          'AY'    ' Sigla do Estado do Favorecido Inválida',
          'AZ'    ' Código/Nome do Banco Depositário Inválido',
          'BA'    ' Código/Nome da Agência Depositária Não Informado',
          'BB'    ' Seu Número Inválido',
          'BC'    ' Nosso Número Inválido',
          'BG'    ' Agência/Conta Impedida Legalmente',
          'BH'    ' Empresa não pagou salário',
          'BI'    ' Falecimento do mutuário',
          'BJ'    ' Empresa não enviou remessa do mutuário',
          'BK'    ' Empresa não enviou remessa no vencimento',
          'BL'    ' Valor da parcela inválida',
          'BM'    ' Identificação do contrato inválida',
          'CA'    ' Código de Barras - Código do Banco Inválido',
          'CB'    ' Código de Barras - Código da Moeda Inválido',
          'CC'    ' Código de Barras - Dígito Verificador Geral Inválido',
          'CD'    ' Código de Barras - Valor do Título Divergente/Inválido',
          'CE'    ' Código de Barras - Campo Livre Inválido',
          'CF'    ' Valor do Documento Inválido',
          'CG'    ' Valor do Abatimento Inválido',
          'CH'    ' Valor do Desconto Inválido',
          'CI'    ' Valor de Mora Inválido',
          'CJ'    ' Valor da Multa Inválido',
          'CK'    ' Valor do IR Inválido',
          'CL'    ' Valor do ISS Inválido',
          'CM'    ' Valor do IOF Inválido',
          'CN'    ' Valor de Outras Deduções Inválido',
          'CO'    ' Valor de Outros Acréscimos Inválido',
          'CP'    ' Valor do INSS Inválido',
          'HA'    ' Lote Não Aceito',
          'HB'    ' Inscrição da Empresa Inválida para o Contrato',
          'HC'    ' Convênio com a Empresa Inexistente/Inválido para o Contrato',
          'HD'    ' Agência/Conta Corrente da Empresa Inexistente/Inválido para o Contrato',
          'HE'    ' Tipo de Serviço Inválido para o Contrato',
          'HF'    ' Conta Corrente da Empresa com Saldo Insuficiente',
          'HG'    ' Lote de Serviço Fora de Sequência',
          'HH'    ' Lote de Serviço Inválido',
          'HI'    ' Arquivo não aceito',
          'HJ'    ' Tipo de Registro Inválido',
          'HK'    ' Código Remessa / Retorno Inválido',
          'HL'    ' Versão de layout inválida',
          'HM'    ' Mutuário não identificado',
          'HN'    ' Tipo do benefício não permite empréstimo',
          'HO'    ' Benefício cessado/suspenso',
          'HP'    ' Benefício possui representante legal',
          'HQ'    ' Benefício é do tipo PA (Pensão alimentícia)',
          'HR'    ' Quantidade de contratos permitida excedida',
          'HS'    ' Benefício não pertence ao Banco informado',
          'HT'    ' Início do desconto informado já ultrapassado',
          'HU'    ' Número da parcela inválida',
          'HV'    ' Quantidade de parcela inválida',
          'HW'    ' Margem consignável excedida para o mutuário dentro do prazo do contrato',
          'HX'    ' Empréstimo já cadastrado',
          'HY'    ' Empréstimo inexistente',
          'HZ'    ' Empréstimo já encerrado',
          'H1'    ' Arquivo sem trailer',
          'H2'    ' Mutuário sem crédito na competência',
          'H3'    ' Não descontado – outros motivos',
          'H4'    ' Retorno de Crédito não pago',
          'H5'    ' Cancelamento de empréstimo retroativo',
          'H6'    ' Outros Motivos de Glosa',
          'H7'    ' Margem consignável excedida para o mutuário acima do prazo do contrato',
          'H8'    ' Mutuário desligado do empregador',
          'H9'    ' Mutuário afastado por licença',
          'IA'    ' Primeiro nome do mutuário diferente do primeiro nome do movimento do censo ou diferente da base de Titular do Benefício',
          'TA'    ' Lote Não Aceito - Totais do Lote com Diferença',
          'YA'    ' Título Não Encontrado',
          'YB'    ' Identificador Registro Opcional Inválido',
          'YC'   	' Código Padrão Inválido',
          'YD'    ' Código de Ocorrência Inválido',
          'YE'    ' Complemento de Ocorrência Inválido',
          'YF'    ' Alegação já Informada',
          'ZA'    ' Agência / Conta do Favorecido Substituída',
          'ZB'    ' Divergência entre o primeiro e último nome do beneficiário versus primeiro e último nome na Receita Federal'.
  ENDIF.

ENDFORM.


FORM motivo USING codigo
                  descricao.
  wa_motivos-codigo    = codigo.
  wa_motivos-descricao = descricao.

  APPEND  wa_motivos TO  t_motivos.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM seleciona_dados.
  DATA lw_lfa1_aux TYPE lfa1.
  DATA lw_bseg     TYPE bseg.
  DATA doc_estorno TYPE bkpf-stblg.

  DATA:
    cod01(10) TYPE c,
    cod02(10) TYPE c,
    cod03(10) TYPE c,
    cod04(10) TYPE c,
    cod05(10) TYPE c.


  READ TABLE list INTO value WITH KEY key = p_banco.

  IF p_recusa IS INITIAL.

    SELECT *
      FROM zhcmt_pa_0009
      INTO TABLE comprovantes
     WHERE bukrs    IN s_bukrs
       AND pernr    IN s_pernr
       AND dt_pgto  IN s_dt_pg
       AND banco    EQ value-text(3)
       AND tp_proce EQ 'A'.
  ELSE.

    SELECT *
      FROM zhcmt_pa_0009
      INTO TABLE comprovantes
     WHERE bukrs    IN s_bukrs
       AND pernr    IN s_pernr
       AND dt_pgto  IN s_dt_pg
       AND banco    EQ value-text(3)
       AND tp_proce EQ 'R'.

  ENDIF.

  IF lines( comprovantes ) > 0.

    SELECT pernr, bukrs, begda, endda, werks
     FROM pa0001
     INTO TABLE @DATA(it_pa0001)
      FOR ALL ENTRIES IN @comprovantes
      WHERE pernr EQ @comprovantes-pernr
        AND begda <= @comprovantes-dt_pgto
        AND endda >= @comprovantes-dt_pgto
        AND bukrs IN @s_bukrs.

    SORT it_pa0001 BY pernr bukrs.

  ENDIF.

  MOVE comprovantes TO comprovantes_aux.

  READ TABLE comprovantes_aux  INTO wa_comprovantes WITH KEY banco = value-text(3).
  IF sy-subrc = 0.
    PERFORM preenche_tab_mot.
  ENDIF.


  LOOP AT comprovantes INTO DATA(_comprovante).


    READ TABLE it_pa0001 ASSIGNING FIELD-SYMBOL(<fs_pa0001>) WITH KEY pernr = _comprovante-pernr
                                                                      bukrs = _comprovante-bukrs
                                                                      BINARY SEARCH.

    IF sy-subrc = 0.
      CLEAR: v_return.

      PERFORM check_authorization USING
                             'P_ORGIN'
                             <fs_pa0001>-werks
                             'PERSA'
                             CHANGING
                             v_return.

      IF ( v_return IS NOT INITIAL ).
        CONTINUE.
      ENDIF.
    ENDIF.

*    PERFORM F_SELECIONA_PAGAMENTO(ZFIS31)
*      USING
*       WL_ZFIT0091-BUKRS
*       WL_ZFIT0091-AUGBL
*      CHANGING
*       WL_ZFIT0091-DT_PGTO
*       LW_BSEG.
*
*    CHECK LW_BSEG-GSBER IN S_WERKS.

*    PERFORM F_CHECAR_PAGAMENTO(ZFIS31)
*     USING
*       WL_ZFIT0091-BUKRS
*       WL_ZFIT0091-AUGBL
*       DOC_ESTORNO.

*    IF ( DOC_ESTORNO IS INITIAL ).
    SELECT SINGLE *
      FROM pa0465
      INTO @DATA(_dados_pessoais)
     WHERE pernr = @_comprovante-pernr.

    SELECT SINGLE rbetr znme1
      FROM reguh
      INTO (wl_saida-vlr_pgto, wl_saida-nome)
     WHERE vblnr = _comprovante-vblnr.

    IF _comprovante-agencia IS NOT INITIAL.
      PERFORM f_strlen USING _comprovante-agencia
                    CHANGING wl_saida-agencia.
    ENDIF.

    IF _comprovante-conta_corrente IS NOT INITIAL.
      PERFORM f_strlen USING _comprovante-conta_corrente
                    CHANGING wl_saida-conta_corrente.
    ENDIF.

    wl_saida-vlr_pgto      = wl_saida-vlr_pgto * -1.
    wl_saida-bukrs         = _comprovante-bukrs.
    wl_saida-cnpj_empresa  = _comprovante-cnpj_empresa.
    wl_saida-vblnr         = _comprovante-vblnr.
    wl_saida-dt_pgto       = _comprovante-dt_pgto.
    wl_saida-banco         = _comprovante-banco.
    wl_saida-matricula     = _comprovante-pernr.
    IF _comprovante-cpf IS INITIAL.
      wl_saida-cpf           = _dados_pessoais-cpf_nr.
    ELSE.
      wl_saida-cpf       = _comprovante-cpf.
    ENDIF.
    wl_saida-bco_fav       = _comprovante-bco_fav.
    wl_saida-cod_autent    = _comprovante-cod_autent.


    cod01 = _comprovante-motivo(2).
    cod02 = _comprovante-motivo+2(2).
    cod03 = _comprovante-motivo+4(2).
    cod04 = _comprovante-motivo+6(2).
    cod05 = _comprovante-motivo+8(2).
    wl_saida-desc_mot = 'ERRO'.

    IF cod02 IS NOT INITIAL.
      CONCATENATE cod01  '|'  cod02  INTO wl_saida-motivo.
    ENDIF.

    IF cod03 IS NOT INITIAL.
      CONCATENATE cod01  '|'  cod02 '|' cod03   INTO wl_saida-motivo.
    ENDIF.

    IF cod04 IS NOT INITIAL.
      CONCATENATE cod01  '|'  cod02 '|' cod03 '|' cod04  INTO wl_saida-motivo.
    ENDIF.

    IF cod05 IS NOT INITIAL.
      CONCATENATE cod01  '|'  cod02 '|' cod03 '|' cod04 '|' cod05 INTO wl_saida-motivo.
    ENDIF.

    IF cod02 IS INITIAL AND  cod03 IS INITIAL AND
       cod04 IS INITIAL AND  cod05 IS INITIAL.
      wl_saida-motivo = cod01.
    ENDIF.

    IF ( _comprovante-dv_ag_fav IS INITIAL ).
      wl_saida-ag_fav = _comprovante-ag_fav.
    ELSE.
      CONCATENATE _comprovante-ag_fav '-' _comprovante-dv_ag_fav INTO wl_saida-ag_fav.
    ENDIF.

    SHIFT _comprovante-cc_fav LEFT DELETING LEADING '0'.

    IF ( _comprovante-dv_fav IS INITIAL ).
      wl_saida-cc_fav = _comprovante-cc_fav.
    ELSE.
      CONCATENATE _comprovante-cc_fav '-' _comprovante-dv_fav INTO wl_saida-cc_fav.
    ENDIF.


    PERFORM mensagem USING wl_saida-motivo.

    APPEND wl_saida TO gt_saida.
    CLEAR: wl_saida,
           wa_motivos,
           cod01,
           cod02,
           cod03,
           cod04,
           cod05.
  ENDLOOP.

  IF NOT ( gt_saida IS INITIAL ).
    PERFORM f_imprime_dados.
  ELSE.
    MESSAGE 'Nenhum comprovante foi encontrado.' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                    "SELECIONA_DADOS

FORM exporta_tabela CHANGING p_t_texto LIKE t_motivo_smart.

  MOVE t_motivo_smart[] TO p_t_texto[].

ENDFORM.

FORM mensagem USING codigo.
  DATA:
    wl_cont TYPE sy-tabix,
    wl_mod  TYPE sy-tabix,
    wl_pos  TYPE sy-tabix,
    wl_line TYPE sy-tabix.

  CLEAR: wa_saida_msg, t_saida_msg.

  LOOP AT t_motivos INTO wa_motivos WHERE codigo = codigo(2).

    wa_motivo_smart-codigo = wa_motivos-codigo.
    wa_motivo_smart-descricao =  wa_motivos-descricao.

    APPEND wa_motivo_smart TO t_motivo_smart.


    wl_cont =  strlen( wa_motivos-descricao ).
    WHILE wl_pos < wl_cont.
      wl_line = wl_cont - wl_pos.

      IF wl_line >= 72.
        wl_line = 72.
      ENDIF.

      wa_saida_msg = wa_motivos-descricao+wl_pos(wl_line).
      ADD 72 TO wl_pos.

      IF wa_saida_msg IS NOT INITIAL.
        APPEND  wa_saida_msg TO   t_saida_msg.
      ENDIF.

      CLEAR:   wa_saida_msg.
    ENDWHILE.
  ENDLOOP.

  CLEAR: wl_cont,
         wl_mod,
         wl_pos,
         wl_line.


  LOOP AT t_motivos INTO wa_motivos WHERE codigo = codigo+3(2).
    wa_motivo_smart-codigo = wa_motivos-codigo.
    wa_motivo_smart-descricao =  wa_motivos-descricao.

    APPEND wa_motivo_smart TO t_motivo_smart.


    wl_cont =  strlen( wa_motivos-descricao ).
    WHILE wl_pos < wl_cont.
      wl_line = wl_cont - wl_pos.

      IF wl_line >= 72.
        wl_line = 72.
      ENDIF.

      wa_saida_msg = wa_motivos-descricao+wl_pos(wl_line).
      ADD 72 TO wl_pos.

      IF wa_saida_msg IS NOT INITIAL.
        APPEND  wa_saida_msg TO   t_saida_msg.
      ENDIF.

      CLEAR:   wa_saida_msg.
    ENDWHILE.
  ENDLOOP.

  CLEAR: wl_cont,
     wl_mod,
     wl_pos,
     wl_line.


  LOOP AT t_motivos INTO wa_motivos WHERE codigo = codigo+6(2).
    wa_motivo_smart-codigo = wa_motivos-codigo.
    wa_motivo_smart-descricao =  wa_motivos-descricao.

    APPEND wa_motivo_smart TO t_motivo_smart.

    wl_cont =  strlen( wa_motivos-descricao ).
    WHILE wl_pos < wl_cont.
      wl_line = wl_cont - wl_pos.

      IF wl_line >= 72.
        wl_line = 72.
      ENDIF.

      wa_saida_msg = wa_motivos-descricao+wl_pos(wl_line).
      ADD 72 TO wl_pos.

      IF wa_saida_msg IS NOT INITIAL.
        APPEND  wa_saida_msg TO   t_saida_msg.
      ENDIF.

      CLEAR:   wa_saida_msg.
    ENDWHILE.
  ENDLOOP.

  CLEAR: wl_cont,
         wl_mod,
         wl_pos,
         wl_line.


  LOOP AT t_motivos INTO wa_motivos WHERE codigo = codigo+9(2).
    wa_motivo_smart-codigo = wa_motivos-codigo.
    wa_motivo_smart-descricao =  wa_motivos-descricao.

    APPEND wa_motivo_smart TO t_motivo_smart.

    wl_cont =  strlen( wa_motivos-descricao ).
    WHILE wl_pos < wl_cont.
      wl_line = wl_cont - wl_pos.

      IF wl_line >= 72.
        wl_line = 72.
      ENDIF.

      wa_saida_msg = wa_motivos-descricao+wl_pos(wl_line).
      ADD 72 TO wl_pos.

      IF wa_saida_msg IS NOT INITIAL.
        APPEND  wa_saida_msg TO   t_saida_msg.
      ENDIF.

      CLEAR:   wa_saida_msg.
    ENDWHILE.
  ENDLOOP.
  CLEAR: wl_cont,
         wl_mod,
         wl_pos,
         wl_line.

  LOOP AT t_motivos INTO wa_motivos WHERE codigo = codigo+12(2).
    wa_motivo_smart-codigo = wa_motivos-codigo.
    wa_motivo_smart-descricao =  wa_motivos-descricao.

    APPEND wa_motivo_smart TO t_motivo_smart.

    wl_cont =  strlen( wa_motivos-descricao ).
    WHILE wl_pos < wl_cont.
      wl_line = wl_cont - wl_pos.

      IF wl_line >= 72.
        wl_line = 72.
      ENDIF.

      wa_saida_msg = wa_motivos-descricao+wl_pos(wl_line).
      ADD 72 TO wl_pos.

      IF wa_saida_msg IS NOT INITIAL.
        APPEND  wa_saida_msg TO   t_saida_msg.
      ENDIF.

      CLEAR:   wa_saida_msg.
    ENDWHILE.
  ENDLOOP.

  CLEAR: wl_cont,
         wl_mod,
         wl_pos,
         wl_line.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_STATUS_001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_status_001 USING pf_tab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD' EXCLUDING pf_tab.
ENDFORM.                    "F_STATUS_001

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_imprime_dados.

  IF p_recusa IS NOT INITIAL.


    PERFORM alv_preenche_cat_slis USING:
    'BUKRS'          'Empresa'                  ''     '' ''  '',
    'AGENCIA'        'Agência'                  ''     '' ''  '',
    'CONTA_CORRENTE' 'Conta Corrente'           ''     '' '' 'X',
    'VBLNR'          'Doc.Pgto'                 ''     '' '' 'X',
    'DT_PGTO'        'Dt Pgto'                  ''     '' ''  '',
    'VLR_PGTO'       'Vlr Pgto'                 ''     '' ''  '',
    'MATRICULA'      'Matricula'                ''     '' '' 'X',
    'NOME'           'Nome'                     '30'   '' ''  '',
    'CPF'            'CPF'                      ''     '' ''  '',
    'BCO_FAV'        'Banco'                    ''     '' ''  '',
    'AG_FAV'         'Agência'                  ''     '' ''  '',
    'CC_FAV'         'C/Corrente'               ''     '' '' 'X',
    'COD_AUTENT'     'Cod Autent'               ''     '' ''  '',
    'MOTIVO'         'Cód. Retorno'             '15'   '' ''  '',
    'DESC_MOT'       'Descrição'                '10'   '' ''  ''.

  ELSE.
    PERFORM alv_preenche_cat_slis USING:
    'BUKRS'          'Empresa'                  ''     '' ''  '',
    'AGENCIA'        'Agência'                  ''     '' ''  '',
    'CONTA_CORRENTE' 'Conta Corrente'           ''     '' '' 'X',
    'VBLNR'          'Doc.Pgto'                 ''     '' '' 'X',
    'DT_PGTO'        'Dt Pgto'                  ''     '' ''  '',
    'VLR_PGTO'       'Vlr Pgto'                 ''     '' ''  '',
    'MATRICULA'      'Matricula'                ''     '' '' 'X',
    'NOME'           'Nome'                     '30'   '' ''  '',
    'CPF'            'CPF'                      ''     '' ''  '',
    'BCO_FAV'        'Banco'                    ''     '' ''  '',
    'AG_FAV'         'Agência'                  ''     '' ''  '',
    'CC_FAV'         'C/Corrente'               ''     '' '' 'X',
    'COD_AUTENT'     'Cod Autent'               ''     '' ''  ''.
  ENDIF.
*  'COD_BARRAS'     'Cod Barras'       '48' '' ''  ''.

  wl_variant-report       = sy-repid.
  wl_layout-box_fieldname = 'CHECK'.



  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_top_of_page   = 'TOP-OF-PAGE'
      i_callback_pf_status_set = 'F_STATUS_001'
      is_layout                = wl_layout
      is_variant               = wl_variant
      i_callback_user_command  = 'F_USER_COMMAND'
      it_fieldcat              = gt_fcat_slis
      i_save                   = 'X'
    TABLES
      t_outtab                 = gt_saida.

ENDFORM.                    "F_IMPRIME_DADOS

*&---------------------------------------------------------------------*
*&      Form  TOP-OF-PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top-of-page.
  DATA: lt_header TYPE slis_listheader,
        gt_header TYPE slis_t_listheader,
        wl_lfa1   TYPE lfa1.


  IF p_recusa IS NOT INITIAL.

    lt_header-typ  = 'H'.
    lt_header-info = text-008.
    APPEND lt_header TO gt_header.
  ELSE.
    lt_header-typ  = 'H'.
    lt_header-info = text-001.
    APPEND lt_header TO gt_header.
  ENDIF.

  SELECT SINGLE *
    FROM t001
    INTO wl_t001
   WHERE bukrs = s_bukrs-low.

  lt_header-typ  = 'S'.
  lt_header-key  = 'Empresa:'.

*  IF ( S_BUKRS-HIGH IS INITIAL ).
  CONCATENATE s_bukrs-low '-' wl_t001-butxt INTO lt_header-info SEPARATED BY space.
*  ELSE.
*    CONCATENATE S_BUKRS-LOW 'Até' S_BUKRS-HIGH INTO LT_HEADER-INFO SEPARATED BY SPACE.
*  ENDIF.
  APPEND lt_header TO gt_header.
  CLEAR lt_header.

  IF ( s_pernr-low IS NOT INITIAL ).

    SELECT SINGLE *
      FROM pa0465
      INTO @DATA(_dados_pessoais)
     WHERE pernr = @s_pernr-low.

    lt_header-typ = 'S'.
    lt_header-key = 'Matricula:'.

    SHIFT s_pernr-low  LEFT DELETING LEADING '0'.
    SHIFT s_pernr-high LEFT DELETING LEADING '0'.

    IF ( s_pernr-high IS INITIAL ).
      CONCATENATE s_pernr-low '-' _dados_pessoais-uname INTO lt_header-info SEPARATED BY space.
    ELSE.
      CONCATENATE s_pernr-high 'Até' s_pernr-high INTO lt_header-info SEPARATED BY space.
    ENDIF.
    APPEND lt_header TO gt_header.
    CLEAR lt_header.

  ENDIF.

  IF ( s_dt_pg-low IS NOT INITIAL ).

    lt_header-typ = 'S'.
    lt_header-key = 'Dt Pagamento:'.

    DATA: date1(10), date2 TYPE char10.

    CONCATENATE s_dt_pg-low+6(2) '/' s_dt_pg-low+4(2) '/' s_dt_pg-low(4) INTO date1.
    CONCATENATE s_dt_pg-high+6(2) '/' s_dt_pg-high+4(2) '/' s_dt_pg-high(4) INTO date2.

    IF ( s_dt_pg-high IS INITIAL ).
      lt_header-info = date1.
    ELSE.
      CONCATENATE date1 'Até' date2 INTO lt_header-info SEPARATED BY space.
    ENDIF.
    APPEND lt_header TO gt_header.
    CLEAR lt_header.

  ENDIF.

  READ TABLE list INTO value WITH KEY key = p_banco.

  lt_header-typ  = 'S'.
  lt_header-key  = 'Banco:'.
  lt_header-info = value-text.

  APPEND lt_header TO gt_header.
  CLEAR lt_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_header.
ENDFORM.                    "TOP-OF-PAGE

*&---------------------------------------------------------------------*
*&      Form  F_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_user_command USING i_ucomm     LIKE sy-ucomm
                          i_selfield  TYPE slis_selfield.
  CASE i_ucomm.
    WHEN c_gerar_comprovante.
      REFRESH comprovantes.

      LOOP AT gt_saida INTO wl_saida WHERE check = 'X'.
        SELECT *
          FROM zhcmt_pa_0009
     APPENDING TABLE comprovantes
         WHERE bukrs = wl_saida-bukrs
           AND vblnr = wl_saida-vblnr.
      ENDLOOP.

      wl_output_opt-tddest  = 'LOCL'.
      wl_output_opt-tdimmed = 'X'.

      PERFORM f_call_smart_forms USING wl_output_opt
                                       wl_control CHANGING wl_job_output.

    WHEN c_enviar_email.
      DATA: v_flag TYPE butxt.
      CLEAR: comprovantes, v_empresas, s_email[].

      LOOP AT gt_saida INTO wl_saida WHERE check = 'X'.

        SELECT *
          FROM zhcmt_pa_0009
     APPENDING TABLE comprovantes
         WHERE bukrs = wl_saida-bukrs
           AND vblnr = wl_saida-vblnr.
      ENDLOOP.

      IF sy-subrc IS INITIAL.
        CALL SCREEN 0200 STARTING AT 8 5
                           ENDING AT 70 6.
      ELSE.
        MESSAGE 'Nenhum comprovante selecionado.' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN '&IC1'.
      PERFORM z_hotspot_report USING i_selfield.

      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title        = 'Erro Recusa'
          im_display_mode = 'X'
        CHANGING
          ch_text         = t_saida_msg.


    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "F_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  F_CALL_SMART_FORMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_call_smart_forms USING i_output_options TYPE ssfcompop
                              i_control_params TYPE ssfctrlop
                        CHANGING
                              c_job_output     TYPE ssfcrescl.

  IF p_recusa IS NOT INITIAL.
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname = c_smart_forms_recusa
      IMPORTING
        fm_name  = v_func_name.
  ELSE.


    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname = c_smart_forms
      IMPORTING
        fm_name  = v_func_name.
  ENDIF.

  CLEAR wl_job_output.

  CALL FUNCTION v_func_name
    EXPORTING
      control_parameters = i_control_params
      output_options     = i_output_options
      user_settings      = ' '
    IMPORTING
      job_output_info    = c_job_output
    TABLES
      it_data            = comprovantes
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
ENDFORM.                    "F_CALL_SMART_FORMS

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT_SLIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_preenche_cat_slis  USING fieldname
                                  seltext_m
                                  output_leng
                                  checkbox
                                  edit
                                  no_zero.


  DATA: wl_fcat_slis TYPE slis_fieldcat_alv.

  wl_fcat_slis-fieldname   = fieldname.
  wl_fcat_slis-seltext_m   = seltext_m.
  wl_fcat_slis-checkbox    = checkbox.
  wl_fcat_slis-edit        = edit.
  wl_fcat_slis-no_zero     = no_zero.
  wl_fcat_slis-outputlen   = output_leng.
  IF fieldname = 'DESC_MOT'.
    wl_fcat_slis-hotspot   = 'X'.
  ENDIF.

  APPEND wl_fcat_slis TO gt_fcat_slis.
  CLEAR wl_fcat_slis.
ENDFORM.                    "ALV_PREENCHE_CAT_SLIS

*&---------------------------------------------------------------------*
*&      Form  F_STRLEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_TEXT     text
*----------------------------------------------------------------------*
FORM f_strlen  CHANGING i_text e_text.
  DATA: v_strlen TYPE i.

  REPLACE '-' WITH '' INTO i_text.
  v_strlen = strlen( i_text ) - 1.

  CONCATENATE i_text(v_strlen) '-' i_text+v_strlen(1)
  INTO e_text.

ENDFORM.                    "F_STRLEN

*&---------------------------------------------------------------------*
*&      Form  F_SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_enviar_email.
  DATA: gw_objtxt   LIKE solisti1   OCCURS 0 WITH HEADER LINE,
        gw_objpack  LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        gw_reclist  LIKE somlreci1  OCCURS 0 WITH HEADER LINE,
        gw_objbin   LIKE solisti1   OCCURS 0 WITH HEADER LINE,

        wl_doc_chng TYPE sodocchgi1,
        wl_objhead  TYPE soli_tab,
        v_lines_txt TYPE i,
        v_lines_bin TYPE i.

  wl_control-no_dialog   = 'X'.
  wl_control-preview     = space.
  wl_control-device      = 'PRINTER'.
  wl_control-getotf      = 'X'.

  wl_output_opt-tddest   = 'LOCL'.
  wl_output_opt-tdimmed  = 'X'.
  wl_output_opt-tdnewid  = 'X'.
  wl_output_opt-tdnoarch = 'X'.

  PERFORM f_call_smart_forms USING wl_output_opt
                                   wl_control CHANGING wl_job_output.

  PERFORM f_generate_pdf USING wl_job_output.

  REFRESH: gw_objtxt,
           gw_objpack.

  gw_objbin[] = gw_record[].

* ______Corpo do E-mail______

  gw_objtxt = text-005. APPEND gw_objtxt.
  gw_objtxt = space.    APPEND gw_objtxt.
  gw_objtxt = text-003. APPEND gw_objtxt.
  gw_objtxt = text-004. APPEND gw_objtxt.
* ___________________________

  DESCRIBE TABLE gw_objtxt LINES v_lines_txt.
  READ TABLE gw_objtxt INDEX v_lines_txt.

  SELECT SINGLE *
    FROM t001
    INTO wl_t001
   WHERE bukrs = s_bukrs-low.

  CONCATENATE 'Comprovante de pagamento - Folha Pgto' wl_t001-butxt INTO wl_doc_chng-obj_descr SEPARATED BY space.
  wl_doc_chng-obj_name   = 'smartforms'.
  wl_doc_chng-sensitivty = 'F'.
  wl_doc_chng-doc_size   = v_lines_txt * 255.

* Main Text
  gw_objpack-head_start = 1.
  gw_objpack-head_num   = 0.
  gw_objpack-body_start = 1.
  gw_objpack-body_num   = v_lines_txt.
  gw_objpack-doc_type   = 'RAW'.
  APPEND gw_objpack.

* Attachment (pdf-Attachment)
  gw_objpack-transf_bin = 'X'.
  gw_objpack-head_start = 1.
  gw_objpack-head_num   = 0.
  gw_objpack-body_start = 1.

  DESCRIBE TABLE gw_objbin LINES v_lines_bin.
  READ TABLE gw_objbin INDEX v_lines_bin.

  gw_objpack-doc_size   = v_lines_bin * 255 .
  gw_objpack-body_num   = v_lines_bin.
  gw_objpack-doc_type   = 'PDF'.
  gw_objpack-obj_name   = 'smart'.
  gw_objpack-obj_descr  = 'Comprovante de pagamento - Folha Pgto'.
  APPEND gw_objpack.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Enviando comprovante(s)...'.

  LOOP AT s_email.
    CLEAR gw_reclist.

    gw_reclist-receiver = s_email-low.
    gw_reclist-rec_type = 'U'.
    APPEND gw_reclist.

    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data              = wl_doc_chng
        put_in_outbox              = 'X'
        commit_work                = 'X'
      TABLES
        packing_list               = gw_objpack
        object_header              = wl_objhead
        contents_bin               = gw_objbin
        contents_txt               = gw_objtxt
        receivers                  = gw_reclist
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.
  ENDLOOP.
ENDFORM.                    "F_SEND_EMAIL

*&---------------------------------------------------------------------*
*&      Form  F_GENERATE_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_generate_pdf USING i_job_info TYPE ssfcrescl.

  DATA: gw_otf         TYPE itcoo OCCURS 0 WITH HEADER LINE,
        v_bin_filesize TYPE i,
        wl_buffer      TYPE string.

  gw_otf[] = i_job_info-otfdata[].

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
      max_linewidth         = 132
    IMPORTING
      bin_filesize          = v_bin_filesize
    TABLES
      otf                   = gw_otf
      lines                 = gw_tline
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      OTHERS                = 4.

  LOOP AT gw_tline.
    TRANSLATE gw_tline USING '~'.
    CONCATENATE wl_buffer gw_tline INTO wl_buffer.
  ENDLOOP.
  TRANSLATE wl_buffer USING '~'.
  DO.
    gw_record = wl_buffer.
    APPEND gw_record.
    SHIFT wl_buffer LEFT BY 255 PLACES.
    IF ( wl_buffer IS INITIAL ).
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    "F_GENERATE_PDF
*&---------------------------------------------------------------------*
*&      Module  PAI_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'ENVIAR'.
      IF s_email IS INITIAL.
        MESSAGE 'Informar um e-mail para o destinatário.' TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        PERFORM f_enviar_email.

        MESSAGE 'Comprovante(s) enviado(s) com sucesso!' TYPE 'I' DISPLAY LIKE 'S'.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " PAI_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '0200'.
  SET TITLEBAR '0200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
