"Name: \PR:SAPMV45A\FO:USEREXIT_SAVE_DOCUMENT\SE:BEGIN\EI
ENHANCEMENT 0 Z_SAVE_ITEMS_ORDER.
***BBKO/André Zorba - Início da Inclusão - 18.07.2010
* Inicialização do workflow de:
*   Limite Crédito- Liberação Ordem de Venda - WS99900006

*** DECLARAÇÕES

* Types
  TYPES: BEGIN OF TY_KNKK,
           KLIMK TYPE KNKK-KLIMK,             "#EC CI_USAGE_OK[2227014]
           SKFOR TYPE KNKK-SKFOR,             "#EC CI_USAGE_OK[2227014]
           SAUFT TYPE KNKK-SAUFT,             "#EC CI_USAGE_OK[2227014]
           SSOBL TYPE KNKK-SSOBL,             "#EC CI_USAGE_OK[2227014]
         END OF TY_KNKK.

* Tabelas e estruturas
  DATA: LT_CONT           TYPE TABLE OF SWR_CONT,
        LW_CONT           TYPE SWR_CONT,
        LW_ORDERHEADERIN  TYPE BAPISDH1,
        LW_ORDERHEADERINX TYPE BAPISDH1X,
        LT_RET_BAPI       TYPE BAPIRET2 OCCURS 0,
        LW_KNKK           TYPE TY_KNKK.

* Variaveis
  DATA: LV_VALOR1 TYPE NETWR_AK, " Limite total
        LV_VALOR2 TYPE NETWR_AK. " Saldo Excendente/Limite de Crédito

* ---> S4 Migration - 18/07/2023 - CA
  DATA: LT_KNKK       TYPE STANDARD TABLE OF KNKK, "#EC CI_USAGE_OK[2227014]
        LT_DATA_WHERE TYPE STANDARD TABLE OF ZKNKK_KEY,
        WA_DATA_WHERE TYPE ZKNKK_KEY.
* <--- S4 Migration - 18/07/2023 - CA

* Constantes
  CONSTANTS: LC_C      TYPE CHAR1 VALUE 'C',
             LC_E      TYPE CHAR1 VALUE 'E',
             LC_VA01   TYPE SY-TCODE   VALUE 'VA01',
             LC_VA02   TYPE SY-TCODE   VALUE 'VA02',
             LC_VALOR1 TYPE CHAR6 VALUE 'VALOR1',
             LC_VALOR2 TYPE CHAR6 VALUE 'VALOR2'.


*** Início do Processamento

  CLEAR: LV_VALOR1, LV_VALOR2, LW_KNKK.

* Selecionar dados -
* Mestre clientes administr.de créditos: dados área controle

* ---> S4 Migration - 18/07/2023 - CA
*  SELECT klimk skfor sauft ssobl
*    UP TO 1 ROWS
*    FROM knkk
*    INTO lw_knkk
*   WHERE kunnr = vbak-kunnr.
*  ENDSELECT.

  CLEAR WA_DATA_WHERE.
  FREE: LT_DATA_WHERE, LT_KNKK.

  WA_DATA_WHERE-KUNNR = VBAK-KUNNR.
  APPEND WA_DATA_WHERE TO LT_DATA_WHERE.

  CALL FUNCTION 'Z_FROM_TO_KNKK'
    TABLES
      T_DATA_WHERE = LT_DATA_WHERE
      T_KNKK       = LT_KNKK.

  IF LT_KNKK[] IS NOT INITIAL.
    READ TABLE LT_KNKK INTO DATA(WA_KNKK) INDEX 1.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING WA_KNKK TO LW_KNKK.
    ENDIF.
  ENDIF.
* <--- S4 Migration - 18/07/2023 - CA

****************************
**** Criação da OV - VA01
****************************
  IF SY-TCODE EQ LC_VA01.

    IF TVAK-KLIMP = LC_C.

      LV_VALOR1 = LW_KNKK-KLIMK - LW_KNKK-SKFOR - LW_KNKK-SAUFT - LW_KNKK-SSOBL.

      IF LV_VALOR1 < VBAK-NETWR.

*     Obtêm VALOR2.
        LV_VALOR2 = VBAK-NETWR - LV_VALOR1.

        IF NOT LV_VALOR2 IS INITIAL.

*       Carrega valores no container
          CLEAR: LW_CONT.
          REFRESH: LT_CONT.
          LW_CONT-ELEMENT = LC_VALOR1.
          LW_CONT-VALUE = LV_VALOR1.
          CONDENSE LW_CONT-VALUE.
          APPEND LW_CONT TO LT_CONT.

          CLEAR LW_CONT.
          LW_CONT-ELEMENT = LC_VALOR2.
          LW_CONT-VALUE = LV_VALOR2.
          CONDENSE LW_CONT-VALUE.
          APPEND LW_CONT TO LT_CONT.

*       Chamada para iniciar um novo workflow.
          CALL FUNCTION 'Z_SD_WF_CHAMAR_EVENTOS'
            EXPORTING
              VBELN           = VBAK-VBELN
              TIPO_EVENTO     = LC_C
            TABLES
              INPUT_CONTAINER = LT_CONT.

        ENDIF.
      ENDIF.
    ENDIF.

****************************
**** Alteração da OV - VA02
****************************
  ELSEIF SY-TCODE EQ LC_VA02.

* Se for uma alteração na OV excluir o WF atual.

* Faz a chamada do evento de encerramento.
    CALL FUNCTION 'Z_SD_WF_CHAMAR_EVENTOS'
      EXPORTING
        VBELN       = VBAK-VBELN
        TIPO_EVENTO = LC_E.    " Excluír


    IF TVAK-KLIMP = LC_C.

      LV_VALOR1 = LW_KNKK-KLIMK - LW_KNKK-SKFOR - LW_KNKK-SAUFT - LW_KNKK-SSOBL.

      IF ( LV_VALOR1 < VBAK-NETWR ).

*     Obtêm VALOR2.
        LV_VALOR2 = VBAK-NETWR - LV_VALOR1.

*     Se o limite de crédito foi ultrapassado.
        IF NOT LV_VALOR2 IS INITIAL.

          CLEAR: LW_CONT.
          REFRESH: LT_CONT.
          LW_CONT-ELEMENT = LV_VALOR1.
          LW_CONT-VALUE = LV_VALOR1.
          APPEND LW_CONT TO LT_CONT.

          CLEAR LW_CONT.
          LW_CONT-ELEMENT = LC_VALOR2.
          LW_CONT-VALUE = LV_VALOR2.
          APPEND LW_CONT TO LT_CONT.

*       Chamada para iniciar um novo workflow.
          CALL FUNCTION 'Z_SD_WF_CHAMAR_EVENTOS'
            EXPORTING
              VBELN           = VBAK-VBELN
              TIPO_EVENTO     = LC_C   " Criar
            TABLES
              INPUT_CONTAINER = LT_CONT.
        ELSE.
**       Efetuar o desbloqueio da OV.
*        w_orderheaderin-dlv_block = space.
*        w_orderheaderinx-dlv_block = 'X'.
*        w_orderheaderinx-updateflag = 'U'.
*
*        CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
*          EXPORTING
*            salesdocument               = vbak-vbeln
*            ORDER_HEADER_IN             = w_orderheaderin
*            order_header_inx            = w_orderheaderinx
*          tables
*            return                      = t_ret_bapi.
*
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
***BBKO/André Zorba - Fim da Inclusão - 18.07.2010


***BBKO/Paulo Barrios - Início da Inclusão - 20.07.2010
  DATA:   BEGIN OF LT_VBAP OCCURS 125.
            INCLUDE STRUCTURE VBAPVB.
  DATA:   END OF LT_VBAP.

  DATA: VL_MTART TYPE MTART,
        WL_MARA  TYPE MARA,
        R_MATKL  TYPE RANGE OF EKPO-MATKL.

  DATA: LT_VBKD        TYPE TABLE OF VBKD,
        LT_VBUK        TYPE TABLE OF VBUK,
        LT_VBPA        TYPE TABLE OF VBPA,
        LT_VALUES      TYPE TABLE OF RGSB4,
        LT_RANGE       TYPE RANGE OF VBAP-WERKS,
        LW_RANGE       LIKE LINE OF LT_RANGE,
        LW_VBAK        TYPE VBAK,
        LW_VBAP        TYPE VBAPVB,
        LW_VBUK        TYPE VBUK,
        LW_VBKD        TYPE VBKD,
        LW_VBPA        TYPE VBPA,
        LV_ATUALIZACAO TYPE CHAR1,
        LW_AUART       TYPE VBAK-AUART,
        LW_STATUS      TYPE VBUK-GBSTK.

  FIELD-SYMBOLS: <FS_DATA> LIKE LINE OF LT_VBPA.

  CONSTANTS: LC_000000 TYPE VBKD-POSNR VALUE '000000',
             LC_14     TYPE CHAR2      VALUE '14',
             LC_01     TYPE CHAR2      VALUE '01',
             LC_02     TYPE CHAR2      VALUE '02',
             LC_A      TYPE CHAR1      VALUE 'A',
             LC_I      TYPE CHAR1      VALUE 'I',
             LC_0      TYPE CHAR1      VALUE '0',
             LC_D      TYPE CHAR1      VALUE 'D'.

  FIELD-SYMBOLS: <FS1_VBAK> TYPE VBAK,
                 <FS1_VBAP> TYPE VBAPVB,
                 <FS1_VBUK> TYPE VBUK,
                 <FS1_VBKD> TYPE VBKD.

  CLEAR LW_AUART.

  DATA: I_ORDEM_VENDA      TYPE ZDE_CARGUEIRO_OV,
        MATKL_FERTILIZANTE TYPE RANGE OF MATKL,
        W_MARA             TYPE MARA,
        T_MATKL_FERT       TYPE TABLE OF TVARVC.

*-CS2020001303 - 10.11.2021 - JT - inicio
*-----------------
*-- Grupo fertilizantes
*-----------------
  FREE: MATKL_FERTILIZANTE.

  SELECT *
    FROM TVARVC
    INTO TABLE T_MATKL_FERT
   WHERE NAME = 'MAGGI_GR_FERTILIZANTES'.

  IF SY-SUBRC = 0.
    LOOP AT T_MATKL_FERT INTO DATA(W_MATKL_FERT).
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = W_MATKL_FERT-LOW ) TO MATKL_FERTILIZANTE.
    ENDLOOP.
  ENDIF.
*-CS2020001303 - 10.11.2021 - JT - fim

  LOOP AT XVBAP ASSIGNING FIELD-SYMBOL(<FS_XVBAP>)." WHERE VBELN IS INITIAL.
    CLEAR: I_ORDEM_VENDA.

*-CS2020001303 - 10.11.2021 - JT - inicio
    CLEAR W_MARA.
    SELECT SINGLE *
      INTO W_MARA
      FROM MARA
     WHERE MATNR = <FS_XVBAP>-MATNR.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = W_MARA-MATKL
      IMPORTING
        OUTPUT = W_MARA-MATKL.

    IF W_MARA-MATKL IN MATKL_FERTILIZANTE[] AND MATKL_FERTILIZANTE[] IS NOT INITIAL.
      CONTINUE.
    ENDIF.
*-CS2020001303 - 10.11.2021 - JT - fim

    MOVE-CORRESPONDING VBAK TO I_ORDEM_VENDA.
    I_ORDEM_VENDA-ITEM = <FS_XVBAP>.
    I_ORDEM_VENDA-PARCEIROS = XVBPA[].
    I_ORDEM_VENDA-COMERCIAL = XVBKD[].
    TRY .
        ZCL_INTEGRACAO_LOTE_FRETE=>ZIF_INTEGRACAO_LOTE_FRETE~SET_GERENCIA_LOTE( EXPORTING I_ORDEM_VENDA = I_ORDEM_VENDA IMPORTING E_ID_LOTE_FRETE = <FS_XVBAP>-ID_LOTE_FRETE ).
      CATCH ZCX_INTEGRACAO.
      CATCH ZCX_ERROR.
    ENDTRY.
  ENDLOOP.

  IF SY-TCODE = LC_VA01 OR
     SY-TCODE = LC_VA02 OR
     SY-TCODE = 'VA41' OR
     SY-TCODE = 'ZSDT0066' OR
     SY-TCODE = 'ZSDT0062' OR
     SY-TCODE = 'ZSDT0123' OR
     SY-TCODE = 'ZLES0077' OR
     SY-TCODE = 'ZSDT0132' OR

     SY-TCODE = 'ZSDT0044' OR
     SY-TCODE = 'ZSDT0087' OR
     SY-TCODE EQ 'ZSDT0081' OR "// US-169490 WBARBOSA 24/07/2025

     SY-TCODE = 'VA42' OR
     SY-CPROG = 'ZSDR0036' OR
     SY-TCODE = 'ZMEMO00'.

*   Atualização Legado
    SELECT SINGLE ZAUART
      FROM ZSDT0010
      INTO LW_AUART
    WHERE  CENARIO EQ 'SD'
      AND  ZAUART  EQ VBAK-AUART.

*    IF vbak-zpesagem = c_01 OR
*       vbak-zpesagem = c_02.
    IF NOT LW_AUART IS INITIAL.

      CLEAR: LV_ATUALIZACAO,
             LW_VBAK,
             LW_VBAP,
             LW_VBUK,
             LW_VBKD.

      REFRESH: LT_VBAP,
               LT_VBKD,
               LT_VBUK.

      LW_VBAK   = VBAK.
      LT_VBAP[] = XVBAP[].
      LT_VBKD[] = XVBKD[].
      LT_VBUK[] = XVBUK[].
      LT_VBPA[] = XVBPA[].

      CASE SY-TCODE.
        WHEN LC_VA01 OR 'VA41'.
          LV_ATUALIZACAO = LC_I.
        WHEN LC_VA02 OR 'VA42'.
          LV_ATUALIZACAO = LC_A.
      ENDCASE.

* Excluir os itens que foram eliminados = D
      DELETE LT_VBAP WHERE UPDKZ EQ LC_D.

* "//Grupo de Marcadoria de Fertilizantes

      SELECT *
        FROM TVARVC
        INTO TABLE T_MATKL_FERT
       WHERE NAME = 'MAGGI_GR_FERTILIZANTES'.

      IF SY-SUBRC IS INITIAL.
        FREE MATKL_FERTILIZANTE.
        LOOP AT T_MATKL_FERT INTO W_MATKL_FERT.
          APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = W_MATKL_FERT-LOW ) TO MATKL_FERTILIZANTE.
        ENDLOOP.
      ENDIF.

* Verificar se o centro está entre exceção
      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          SETNR      = '0000MV45AFZZ_WERKS'
        TABLES
          SET_VALUES = LT_VALUES.

      LW_RANGE-OPTION = 'EQ'.
      LW_RANGE-SIGN   = 'I'.

      LOOP AT LT_VALUES INTO DATA(_VALUES).
        LW_RANGE-LOW = _VALUES-FROM.
        APPEND LW_RANGE TO LT_RANGE.
      ENDLOOP.

* Não precisa passar o vbeln porque a ordem é a mesma
      LOOP AT LT_VBAP ASSIGNING <FS1_VBAP>.

        LW_VBAP = <FS1_VBAP>.

        IF LW_VBAP-WERKS IN LT_RANGE AND LT_RANGE IS NOT INITIAL.

          SELECT SINGLE *
            FROM MARA
            INTO WL_MARA
           WHERE MATNR EQ LW_VBAP-MATNR.

          IF WL_MARA-MTART NE 'ZHAW'.
            IF MATKL_FERTILIZANTE IS NOT INITIAL AND WL_MARA-MATKL IN MATKL_FERTILIZANTE.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.

*&-------------------------------------------------------BUG SOLTO 144473/IR144473 /AOENNING
        DATA: LWA_TVARV_LOTE_MAT TYPE TVARVC.
        SELECT SINGLE *
          FROM TVARVC INTO LWA_TVARV_LOTE_MAT
         WHERE NAME EQ 'ZSDT0191_OV_MAT_LOTE'
           AND LOW  EQ LW_VBAP-MATNR.

        IF SY-SUBRC EQ 0 AND STRLEN( LW_VBAP-CHARG ) > 4.
          LW_VBAP-CHARG = LW_VBAP-CHARG+0(4).
        ENDIF.

*&-------------------------------------------------------BUG SOLTO 144473/IR144473 /AOENNING

*        CHECK ( LW_VBAP-WERKS NOT IN LT_RANGE AND ).

        CLEAR LW_VBUK.
        READ TABLE LT_VBUK ASSIGNING <FS1_VBUK> INDEX 1.

        IF SY-SUBRC IS INITIAL.
          LW_VBUK = <FS1_VBUK>.
        ENDIF.

        CLEAR LW_VBKD.
        READ TABLE LT_VBKD
                   ASSIGNING <FS1_VBKD>
                             WITH KEY POSNR = <FS1_VBAP>-POSNR.

        IF SY-SUBRC IS INITIAL.
          LW_VBKD = <FS1_VBKD>.
        ELSE.  " nivel de Cabeçalho
          READ TABLE LT_VBKD
                 ASSIGNING <FS1_VBKD>
                           WITH KEY  POSNR = LC_000000.

          IF SY-SUBRC IS INITIAL.
            LW_VBKD = <FS1_VBKD>.
          ENDIF.
        ENDIF.

        SORT LT_VBPA BY PARVW ASCENDING.
        READ TABLE LT_VBPA INTO LW_VBPA
          WITH KEY PARVW = 'LR'
          BINARY SEARCH.

        IF SY-SUBRC IS INITIAL.
          LW_VBAK-KUNNR = LW_VBPA-KUNNR.
        ENDIF.

        "tratamento necessário para forçar a atualização do numero da OV quando inclui,
        "pois na tabela interna de parceiro o VBELN ainda não esta preenchido
        LOOP AT LT_VBPA ASSIGNING <FS_DATA>.
          <FS_DATA>-VBELN = LW_VBAK-VBELN.
        ENDLOOP.

        IF NOT ( LW_VBAK-LIFSK IS INITIAL ).
          LW_STATUS     = 'B'.
        ELSE.
          LW_STATUS     = 'A'.
        ENDIF.

        DATA(LWA_FERT_PROD) = SPACE.

*--> 24.08.2023 18:41:16 - Migração S4 – ML - Início
*        CALL FUNCTION 'Z_SD_OUTBOUND_ORD_VENDA'
*          IN BACKGROUND TASK DESTINATION 'XI_ORDEM_VENDA'
*          EXPORTING
*            nu_ordem_venda = lw_vbak-vbeln
*            tp_ordem_venda = lw_vbak-auart
*            nu_item        = lw_vbap-posnr
*            dt_ordem_venda = lw_vbak-erdat
*            tp_frete       = lw_vbkd-inco1
*            id_cliente     = lw_vbak-kunnr
*            qt_ordem_venda = lw_vbap-kwmeng
*            cd_material    = lw_vbap-matnr
*            vr_unitario    = lw_vbap-netpr
*            cd_safra       = lw_vbap-charg
*            cd_empresa     = lw_vbak-vkorg
*            cd_centro      = lw_vbap-werks
*            cd_moeda       = lw_vbap-waerk
*            st_atualizacao = lv_atualizacao
*            status         = lw_status
*            dt_atualizacao = sy-datum
*            hr_atualizacao = sy-uzeit
*            rg_atualizado  = lc_0
*            id_interface   = lc_14
*            transgenia     = lw_vbak-kvgr3
*            id_lote_frete  = lw_vbap-id_lote_frete
*            fert_producao  = lwa_fert_prod
*          TABLES
*            it_vbpa        = lt_vbpa.

        "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223 - INICIO
        ZCL_EUDR_UTILS=>CHECK_OV_PEDIDO_EUDR(
          EXPORTING
            I_VBELN          = LW_VBAK-VBELN     " Nº do documento de compras
            I_CK_LGORT_PARAM = ABAP_TRUE
            I_LGORT          = LW_VBAP-LGORT
          RECEIVING
            R_EUDR           = DATA(V_EUDR)               " Atende critérios Europeu
        ).
        "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223 - FIM
**<<<------"188424 - NMS - INI------>>>
* Verifica se o Tipo da OV é de Prestação de Serviço de frete.
        IF LW_VBAK-AUART EQ 'ZTER'.
          LW_VBAP-CHARG = LW_VBAK-AEDAT(4).

        ENDIF.
**<<<------"188424 - NMS - FIM------>>>
        DATA: LV_RFC TYPE RFCDEST.

        CONSTANTS: C_FM TYPE RS38L_FNAM VALUE 'Z_SD_OUTBOUND_ORD_VENDA'.

        CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
          EXPORTING
            I_FM          = C_FM
          IMPORTING
            E_RFC         = LV_RFC
          EXCEPTIONS
            NO_RFC        = 1
            NO_RFC_CONFIG = 2
            OTHERS        = 3.

        IF SY-SUBRC EQ 0.
          CALL FUNCTION C_FM IN BACKGROUND TASK
            DESTINATION LV_RFC
            EXPORTING
              NU_ORDEM_VENDA = LW_VBAK-VBELN
              TP_ORDEM_VENDA = LW_VBAK-AUART
              NU_ITEM        = LW_VBAP-POSNR
              DT_ORDEM_VENDA = LW_VBAK-ERDAT
              TP_FRETE       = LW_VBKD-INCO1
              ID_CLIENTE     = LW_VBAK-KUNNR
              QT_ORDEM_VENDA = LW_VBAP-KWMENG
              CD_MATERIAL    = LW_VBAP-MATNR
              VR_UNITARIO    = LW_VBAP-NETPR
              CD_SAFRA       = LW_VBAP-CHARG
              CD_EMPRESA     = LW_VBAK-VKORG
              CD_CENTRO      = LW_VBAP-WERKS
              CD_MOEDA       = LW_VBAP-WAERK
              ST_ATUALIZACAO = LV_ATUALIZACAO
              STATUS         = LW_STATUS
              DT_ATUALIZACAO = SY-DATUM
              HR_ATUALIZACAO = SY-UZEIT
              RG_ATUALIZADO  = LC_0
              ID_INTERFACE   = LC_14
              TRANSGENIA     = LW_VBAK-KVGR3
              ID_LOTE_FRETE  = LW_VBAP-ID_LOTE_FRETE
              FERT_PRODUCAO  = LWA_FERT_PROD
              TIPO_DOCUMENTO = 'OV'   "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
              EUDR           = V_EUDR "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
            TABLES
              IT_VBPA        = LT_VBPA.
        ELSE.
          CALL FUNCTION C_FM IN BACKGROUND TASK
            EXPORTING
              NU_ORDEM_VENDA = LW_VBAK-VBELN
              TP_ORDEM_VENDA = LW_VBAK-AUART
              NU_ITEM        = LW_VBAP-POSNR
              DT_ORDEM_VENDA = LW_VBAK-ERDAT
              TP_FRETE       = LW_VBKD-INCO1
              ID_CLIENTE     = LW_VBAK-KUNNR
              QT_ORDEM_VENDA = LW_VBAP-KWMENG
              CD_MATERIAL    = LW_VBAP-MATNR
              VR_UNITARIO    = LW_VBAP-NETPR
              CD_SAFRA       = LW_VBAP-CHARG
              CD_EMPRESA     = LW_VBAK-VKORG
              CD_CENTRO      = LW_VBAP-WERKS
              CD_MOEDA       = LW_VBAP-WAERK
              ST_ATUALIZACAO = LV_ATUALIZACAO
              STATUS         = LW_STATUS
              DT_ATUALIZACAO = SY-DATUM
              HR_ATUALIZACAO = SY-UZEIT
              RG_ATUALIZADO  = LC_0
              ID_INTERFACE   = LC_14
              TRANSGENIA     = LW_VBAK-KVGR3
              ID_LOTE_FRETE  = LW_VBAP-ID_LOTE_FRETE
              FERT_PRODUCAO  = LWA_FERT_PROD
              TIPO_DOCUMENTO = 'OV'   "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
              EUDR           = V_EUDR "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
            TABLES
              IT_VBPA        = LT_VBPA.
        ENDIF.

        COMMIT WORK.
*<-- 24.08.2023 18:41:16 - Migração S4 – ML – Fim


        CLEAR: LT_VBAP,
               LT_VBUK,
               LT_VBKD.

      ENDLOOP.
    ENDIF.
  ENDIF.

***BBKO/Paulo Barrios - Fim    da Inclusão - 20.07.2010

* Vendas Frame - Inicio
*  DATA LW_VBAP TYPE VBAPVB.
  DATA: SL_EXPORT_AUX       TYPE ZSDT_EXPORT.
  READ TABLE XVBAP INTO LW_VBAP INDEX 1.


  IF SY-TCODE EQ 'VA01' OR
     SY-TCODE EQ 'VA02' OR
     SY-TCODE EQ 'ZSDT0066' OR
     SY-TCODE EQ 'ZLES0077' OR
     SY-TCODE EQ 'ZMEMO00'.

    IF VBAK-AUART EQ 'ZFRM' AND
       VBAP-ABGRU EQ SPACE.
      UPDATE ZSDT0021
         SET ORDEM_VENDA = VBAK-VBELN
       WHERE VBELN       EQ CVBAK-VBELN
         AND DTA_PRECO   EQ CVBAK-ERDAT
         AND ORDEM_VENDA EQ SPACE.
    ENDIF.

    IF VBAK-AUART EQ 'ZFRM' AND
       VBAP-ABGRU NE SPACE.
      UPDATE ZSDT0021
         SET ORDEM_VENDA = SPACE
       WHERE VBELN       EQ CVBAK-VBELN
         AND DTA_PRECO   EQ CVBAK-ERDAT
         AND ORDEM_VENDA EQ VBAK-VBELN.
    ENDIF.

    IMPORT SL_EXPORT_AUX FROM MEMORY ID 'ZSDT'.
    FREE MEMORY ID 'ZSDT'.
    IF NOT SL_EXPORT_AUX IS INITIAL.
      UPDATE ZSDT_EXPORT
         SET ORDEM  = VBAK-VBELN
             EXPORT = 'X'
       WHERE DOCNUM     EQ SL_EXPORT_AUX-DOCNUM
         AND NF_RETORNO EQ SL_EXPORT_AUX-NF_RETORNO
         AND WERKS      EQ SL_EXPORT_AUX-WERKS
         AND MATNR      EQ SL_EXPORT_AUX-MATNR.
    ENDIF.

  ENDIF.
* Vendas Frame - Fim

*}   INSERT
* Example:
* CALL FUNCTION 'ZZ_EXAMPLE'
*      IN UPDATE TASK
*      EXPORTING
*           ZZTAB = ZZTAB.
*  TKOMP-zzfield = xxxx-zzfield2.

  EXIT.

ENDENHANCEMENT.
