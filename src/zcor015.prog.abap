*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 20/12/2013                                              &*
*& Descrição: Interface Controle de frotas                            &*
*& Transação: ZCO0021                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*

REPORT  zcor015.
TABLES: coep, bsis.
*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_coas,
         bukrs TYPE coas-bukrs,
         aufnr TYPE coas-aufnr,
         kostl TYPE coas-kostl,
         erdat TYPE coas-erdat,
         gsber TYPE coas-gsber,
         werks TYPE coas-werks,
       END OF ty_coas,

       BEGIN OF ty_afru,
         aufnr TYPE afru-aufnr,
         ersda TYPE afru-ersda,
         rueck TYPE afru-rueck,
         rmzhl TYPE afru-rmzhl,
         werks TYPE afru-werks,
         ismnw TYPE afru-ismnw,
         ismne TYPE afru-ismne,
         pernr TYPE afru-pernr,
       END OF ty_afru,

       BEGIN OF ty_afih,
         aufnr TYPE afih-aufnr,
         equnr TYPE afih-equnr,
       END OF ty_afih,

       BEGIN OF ty_equi,
         equnr TYPE equi-equnr,
         eqtyp TYPE equi-eqtyp,
       END OF ty_equi,

       BEGIN OF ty_saida,
         no_oservico     TYPE afru-aufnr,
         fg_origem(1),
         fg_tipo(1),
         cd_clasmanut(3),
         cd_equipto      TYPE afih-equnr,
         cd_ccusto       TYPE coas-kostl,
         dt_entrada      TYPE coas-erdat,
         unidade_adm     TYPE afru-werks,
         cd_funcionario  TYPE afru-pernr,
         cd_operacao(6),
         dt_inicio       TYPE afru-ersda,
         qt_hr_total     TYPE afru-ismnw,
       END OF ty_saida.
*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA: tg_coas     TYPE TABLE OF ty_coas,
      tg_afru     TYPE TABLE OF ty_afru,
      tg_afih     TYPE TABLE OF ty_afih,
      tg_setleaf  TYPE TABLE OF setleaf,
      tg_zpmr0004 TYPE TABLE OF zpmr0004,
      tg_equi     TYPE TABLE OF ty_equi,
      tg_saida    TYPE TABLE OF ty_saida,
      tg_return   TYPE TABLE OF zpme_return_controle_frota,
      tg_return2  TYPE TABLE OF zpme_return_controle_frota.
*----------------------------------------------------------------------*
* WORK AREA/Variaveis
*----------------------------------------------------------------------*
DATA: wg_coas     TYPE ty_coas,
      wg_afru     TYPE ty_afru,
      wg_afih     TYPE ty_afih,
      wg_setleaf  TYPE setleaf,
      wg_zpmr0004 TYPE zpmr0004,
      wg_equi     TYPE ty_equi,
      wg_saida    TYPE ty_saida,
      wg_return   TYPE zpme_return_controle_frota,
      x_mes       TYPE coep-perio,
      vlines      TYPE i,
      tabix       TYPE sy-tabix.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs FOR coep-bukrs NO-EXTENSION NO INTERVALS OBLIGATORY ,
                  s_perio FOR coep-perio NO-EXTENSION NO INTERVALS OBLIGATORY ,
                  s_gjahr FOR bsis-gjahr NO-EXTENSION NO INTERVALS OBLIGATORY .
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_del  AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK b2.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  x_mes = sy-datum+4(2).
  SUBTRACT 1 FROM x_mes.

  PERFORM selecionar_dados.
  PERFORM organizar_dados.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecionar_dados .

  FREE tg_coas.

  DATA: vl_data_i TYPE sy-datum,
        vl_data_f TYPE sy-datum.

  vl_data_i = |{ s_gjahr-low }{ s_perio-low+1 }01|.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = vl_data_i
    IMPORTING
      last_day_of_month = vl_data_f.


  SELECT bukrs aufnr kostl erdat gsber werks
     FROM coas
     INTO TABLE tg_coas
    WHERE autyp EQ '30'
      AND bukrs IN s_bukrs.

  IF tg_coas IS NOT INITIAL.

    IF tg_coas[] IS NOT INITIAL.
      SELECT *
        INTO TABLE tg_zpmr0004
        FROM zpmr0004.

      SELECT aufnr ersda rueck rmzhl werks ismnw ismne pernr
        FROM afru
        INTO TABLE tg_afru
         FOR ALL ENTRIES IN tg_coas
       WHERE aufnr EQ tg_coas-aufnr
         AND stokz EQ space
         AND stzhl EQ space
         AND budat BETWEEN vl_data_i AND vl_data_f.

      SELECT aufnr equnr
        FROM afih
        INTO TABLE tg_afih
         FOR ALL ENTRIES IN tg_coas
       WHERE aufnr EQ tg_coas-aufnr.

      SELECT DISTINCT equnr eqtyp
        INTO TABLE tg_equi
        FROM equi
         FOR ALL ENTRIES IN tg_afih
       WHERE equnr EQ tg_afih-equnr.

    ENDIF.

  ENDIF.

ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM organizar_dados .
  SORT: tg_coas BY aufnr,
        tg_afih BY aufnr.

  DATA: qtd        TYPE sy-tabix,
        vl_fg_tipo TYPE zpme_return_controle_frota-fg_tipo.

  LOOP AT tg_coas INTO wg_coas.

    zcl_co_utils=>converte_ccusto_agro_otelhar( CHANGING c_kostl = wg_coas-kostl
                                                         c_werks = wg_coas-werks ).

    READ TABLE tg_afih INTO wg_afih WITH KEY aufnr = wg_coas-aufnr BINARY SEARCH.

    IF wg_afih-equnr IS INITIAL.
      vl_fg_tipo = 'C'.

    ELSE.
      CLEAR: wg_equi, wg_zpmr0004.
** Seta a ordem para o centro de custo
      vl_fg_tipo = 'C'.
** Buscar categoria de equipamento
      READ TABLE tg_equi INTO wg_equi WITH KEY equnr = wg_afih-equnr.
      IF sy-subrc IS INITIAL.
** Verifica para quem deve ser gerada a ordem(equipmamento/c.custo) na tabela de parâmetros
        READ TABLE tg_zpmr0004 INTO wg_zpmr0004 WITH KEY eqtyp = wg_equi-eqtyp.
        IF sy-subrc IS INITIAL.
** Verificar se a ordem a ser criada é para o equipamento
          IF wg_zpmr0004-ordem_equip IS NOT INITIAL.
            vl_fg_tipo = 'E'.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    IF NOT line_exists( tg_afru[ aufnr = wg_coas-aufnr ] ).

      MOVE: wg_coas-aufnr   TO wg_return-no_oservico,
            'I'             TO wg_return-fg_origem,
            '100'           TO wg_return-cd_clasmanut,
            wg_afih-equnr   TO wg_return-cd_equipto,
            wg_coas-kostl   TO wg_return-cd_ccusto,
            wg_coas-erdat   TO wg_return-dt_entrada,
            wg_coas-werks   TO wg_return-unidade_adm,
            'SAP/PM'        TO wg_return-cd_operacao,
            '20000101'      TO wg_return-dt_inicio,"Quando chegar no sigam com data 01/01/2000 não ira gerar os itens.
            vl_fg_tipo      TO wg_return-fg_tipo.

      APPEND wg_return TO tg_return.

    ELSE.
      LOOP AT tg_afru INTO wg_afru WHERE  aufnr = wg_coas-aufnr.

        MOVE: wg_afru-aufnr   TO wg_return-no_oservico,
              'I'             TO wg_return-fg_origem,
              '100'           TO wg_return-cd_clasmanut,
              wg_afih-equnr   TO wg_return-cd_equipto,
              wg_coas-kostl   TO wg_return-cd_ccusto,
              wg_coas-erdat   TO wg_return-dt_entrada,
              wg_coas-werks   TO wg_return-unidade_adm,
              wg_afru-pernr   TO wg_return-cd_funcionario,
              'SAP/PM'        TO wg_return-cd_operacao,
              wg_afru-ersda   TO wg_return-dt_inicio,
              wg_afru-ismnw   TO wg_return-qt_hr_total,
              vl_fg_tipo      TO wg_return-fg_tipo..


        APPEND wg_return TO tg_return.
        CLEAR: wg_return.

      ENDLOOP.

    ENDIF.

    CLEAR: wg_return, wg_coas, wg_afih.

  ENDLOOP.

  CLEAR: qtd.
  DESCRIBE TABLE tg_return LINES qtd.


  REFRESH tg_return2.
  CLEAR vlines.
  WHILE 1 = 1.
    CLEAR vlines.
    LOOP AT tg_return INTO wg_return.
      tabix = sy-tabix.
      APPEND wg_return TO tg_return2.
      wg_return-cd_operacao = '999998'.
      MODIFY tg_return FROM wg_return INDEX tabix TRANSPORTING cd_operacao.
      ADD 1 TO vlines.
      IF vlines GT 100000.
        EXIT.
      ENDIF.
    ENDLOOP.
    DELETE tg_return WHERE cd_operacao = '999998'.

*--> 25.08.2023 14:50:36 - Migração S4 – ML - Início
*    CALL FUNCTION 'ZPM_OUTBOUND_CONTROLE_FROTA' IN BACKGROUND TASK
*      DESTINATION 'XI_CONTROLE_FROTA'
*      TABLES
*        t_controle_frota = tg_return2.
*
*    COMMIT WORK.

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'ZPM_OUTBOUND_CONTROLE_FROTA'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        TABLES
          t_controle_frota = tg_return2.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          t_controle_frota = tg_return2.
    ENDIF.

    COMMIT WORK.
*<-- 25.08.2023 14:50:36 - Migração S4 – ML – Fim

    REFRESH tg_return2.
    IF tg_return[] IS INITIAL.
      EXIT.
    ENDIF.
  ENDWHILE.



  MESSAGE s000(z01) WITH qtd 'Registro(s) Enviados com Sucesso. '.


ENDFORM.                    " ORGANIZAR_DADOS
