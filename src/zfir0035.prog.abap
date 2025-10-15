*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 01/10/2013                                              &*
*& Descrição: Relatório Variação Cambial - Competência                &*
*& Transação: ZFI0033                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT  ZFIR0035.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS, KKBLO.

TYPES: BEGIN OF TY_ZGL012_AVM,
        BUKRS             TYPE ZGL012_AVM-BUKRS,
        DT_AVAL           TYPE ZGL012_AVM-DT_AVAL,
        KUNNR             TYPE ZGL012_AVM-KUNNR,
        LIFNR             TYPE ZGL012_AVM-LIFNR,
        HKONT             TYPE ZGL012_AVM-HKONT,
        BELNR             TYPE ZGL012_AVM-BELNR,
        BUZEI             TYPE ZGL012_AVM-BUZEI,
        BSCHL             TYPE ZGL012_AVM-BSCHL,
        UMSKZ             TYPE ZGL012_AVM-UMSKZ,
        BUDAT             TYPE ZGL012_AVM-BUDAT,
        WAERS             TYPE ZGL012_AVM-WAERS,
        GSBER             TYPE ZGL012_AVM-GSBER,
        DMBTR             TYPE ZGL012_AVM-DMBTR,
        WRBTR             TYPE ZGL012_AVM-WRBTR,
        DMBE2             TYPE ZGL012_AVM-DMBE2,
        KURSF             TYPE ZGL012_AVM-KURSF,
        AUGDT             TYPE ZGL012_AVM-AUGDT,
        AUGBL             TYPE ZGL012_AVM-AUGBL,
        TX_FECH           TYPE ZGL012_AVM-TX_FECH,
        VLR_ATUALIZADO    TYPE ZGL012_AVM-VLR_ATUALIZADO,
        VLR_ACUM_MES_ANT  TYPE ZGL012_AVM-VLR_ACUM_MES_ANT,
        VLR_ACUM_MES_ATU  TYPE ZGL012_AVM-VLR_ACUM_MES_ATU,
        VLR_VARIACAO      TYPE ZGL012_AVM-VLR_VARIACAO,
        RESULTADO         TYPE ZGL012_AVM-RESULTADO,
        DT_LCTO           TYPE ZGL012_AVM-DT_LCTO,
        DOC_LCTO          TYPE ZGL012_AVM-DOC_LCTO,
        ESTORNO           TYPE ZGL012_AVM-ESTORNO,
        OBJ_KEY           TYPE ZGL012_AVM-OBJ_KEY,
        CPUDT             TYPE ZGL012_AVM-CPUDT,
        USNAM             TYPE ZGL012_AVM-USNAM,
        ST_REV            TYPE ZGL012_AVM-ST_REV,
       END OF TY_ZGL012_AVM,

       BEGIN OF TY_KNA1,
         KUNNR TYPE KNA1-KUNNR,
         NAME1 TYPE KNA1-NAME1,
       END OF TY_KNA1,

       BEGIN OF TY_LFA1,
         LIFNR TYPE LFA1-LIFNR,
         NAME1 TYPE LFA1-NAME1,
       END OF TY_LFA1,

       BEGIN OF TY_SKAT,
         SAKNR TYPE SKAT-SAKNR,
         TXT20 TYPE SKAT-TXT20,
       END OF TY_SKAT,

       BEGIN OF TY_CONTABIL_CHV,
         OBJ_KEY TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
         BUKRS   TYPE ZIB_CONTABIL_CHV-BUKRS,
         BELNR   TYPE ZIB_CONTABIL_CHV-BELNR,
         GJAHR   TYPE ZIB_CONTABIL_CHV-GJAHR,
       END OF TY_CONTABIL_CHV,

       BEGIN OF TY_BSIS,
         BUKRS TYPE BSIS-BUKRS,
         BELNR TYPE BSIS-BELNR,
         GJAHR TYPE BSIS-GJAHR,
         HKONT TYPE BSIS-HKONT,
         BLDAT TYPE BSIS-BLDAT,
         XBLNR TYPE BSIS-XBLNR,
         BLART TYPE BSIS-BLART,
         ZUONR TYPE BSIS-ZUONR,
       END OF TY_BSIS,

       BEGIN OF TY_BSID,
         BUKRS  TYPE BSID-BUKRS,
         BELNR  TYPE BSID-BELNR,
         GJAHR  TYPE BSID-GJAHR,
         KUNNR  TYPE BSID-KUNNR,
         BLDAT  TYPE BSID-BLDAT,
         XBLNR  TYPE BSID-XBLNR,
         BLART  TYPE BSID-BLART,
         ZUONR  TYPE BSID-ZUONR,
       END OF TY_BSID,

       BEGIN OF TY_BSAD,
         BUKRS TYPE BSAD-BUKRS,
         BELNR TYPE BSAD-BELNR,
         GJAHR TYPE BSAD-GJAHR,
         KUNNR TYPE BSID-KUNNR,
         BLDAT TYPE BSAD-BLDAT,
         XBLNR TYPE BSAD-XBLNR,
         BLART TYPE BSAD-BLART,
         ZUONR TYPE BSAD-ZUONR,
       END OF TY_BSAD,

       BEGIN OF TY_BSIK,
         BUKRS TYPE BSIK-BUKRS,
         BELNR TYPE BSIK-BELNR,
         GJAHR TYPE BSIK-GJAHR,
         LIFNR TYPE BSIK-LIFNR,
         BLDAT TYPE BSIK-BLDAT,
         XBLNR TYPE BSIK-XBLNR,
         BLART TYPE BSIK-BLART,
         ZUONR TYPE BSIK-ZUONR,
       END OF TY_BSIK,

       BEGIN OF TY_BSAK,
         BUKRS TYPE BSAK-BUKRS,
         BELNR TYPE BSAK-BELNR,
         GJAHR TYPE BSAK-GJAHR,
         LIFNR TYPE BSAK-LIFNR,
         BLDAT TYPE BSAK-BLDAT,
         XBLNR TYPE BSAK-XBLNR,
         BLART TYPE BSAK-BLART,
         ZUONR TYPE BSAK-ZUONR,
       END OF TY_BSAK,

       BEGIN OF TY_BSAS,
         BUKRS TYPE BSAS-BUKRS,
         BELNR TYPE BSAS-BELNR,
         GJAHR TYPE BSAS-GJAHR,
         HKONT TYPE BSAS-HKONT,
         BLDAT TYPE BSAS-BLDAT,
         XBLNR TYPE BSAS-XBLNR,
         BLART TYPE BSAS-BLART,
         ZUONR TYPE BSAS-ZUONR,
       END OF TY_BSAS,

       BEGIN OF TY_SAIDA,
        BUKRS            TYPE ZGL012_AVM-BUKRS,
        TIPO(10),
        CODIGO           TYPE KNA1-KUNNR,
        NAME1            TYPE KNA1-NAME1,
        HKONT            TYPE ZGL012_AVM-HKONT,
        BELNR            TYPE ZGL012_AVM-BELNR,
        GJAHR            TYPE BSIS-GJAHR,
        XBLNR            TYPE BSAD-XBLNR,
        BUDAT            TYPE ZGL012_AVM-BUDAT,
        DMBTR            TYPE ZGL012_AVM-DMBTR,
        DMBE2            TYPE ZGL012_AVM-DMBE2,
        WRBTR            TYPE ZGL012_AVM-WRBTR,
        KURSF            TYPE ZGL012_AVM-KURSF,
        WAERS            TYPE ZGL012_AVM-WAERS,
        AUGBL            TYPE ZGL012_AVM-AUGBL,
        AUGDT            TYPE ZGL012_AVM-AUGDT,
        DOC_VARIACAO     TYPE ZIB_CONTABIL_CHV-BELNR,
        VLR_ACUM_MES_ANT TYPE ZGL012_AVM-VLR_ACUM_MES_ANT,
        VLR_VARIACAO     TYPE ZGL012_AVM-VLR_VARIACAO,
        VLR_ACUM_MES_ATU TYPE ZGL012_AVM-VLR_ACUM_MES_ATU,
        ZUONR            TYPE BSAD-ZUONR,
        BLDAT            TYPE BSAD-BLDAT,
        BLART            TYPE BSAD-BLART,
        CT_VAR           TYPE BSIS-HKONT,
        DT_AVAL          TYPE ZGL012_AVM-DT_AVAL,
       END OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA: T_ZGL012_AVM     TYPE TABLE OF TY_ZGL012_AVM,
      T_AVM_AUX        TYPE TABLE OF TY_ZGL012_AVM,
      T_ZGL012_AVM_AUX TYPE TABLE OF TY_ZGL012_AVM,
      T_KNA1           TYPE TABLE OF TY_KNA1,
      T_LFA1           TYPE TABLE OF TY_LFA1,
      T_SKAT           TYPE TABLE OF TY_SKAT,
      T_CONTABIL_CHV   TYPE TABLE OF TY_CONTABIL_CHV,
      T_BSIS           TYPE TABLE OF TY_BSIS,
      T_BSIS_VAR       TYPE TABLE OF TY_BSIS,
      T_BSID           TYPE TABLE OF TY_BSID,
      T_BSAD           TYPE TABLE OF TY_BSAD,
      T_BSIK           TYPE TABLE OF TY_BSIK,
      T_BSAK           TYPE TABLE OF TY_BSAK,
      T_BSAS           TYPE TABLE OF TY_BSAS,
      T_SAIDA          TYPE TABLE OF TY_SAIDA.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_ZGL012_AVM     TYPE TY_ZGL012_AVM,
*      WA_AVM_AUX        TYPE TY_ZGL012_AVM,
      WA_ZGL012_AVM_AUX TYPE TY_ZGL012_AVM,
      WA_KNA1           TYPE TY_KNA1,
      WA_LFA1           TYPE TY_LFA1,
      WA_SKAT           TYPE TY_SKAT,
      WA_CONTABIL_CHV   TYPE TY_CONTABIL_CHV,
      WA_BSIS           TYPE TY_BSIS,
      WA_BSIS_VAR       TYPE TY_BSIS,
      WA_BSID           TYPE TY_BSID,
      WA_BSAD           TYPE TY_BSAD,
      WA_BSIK           TYPE TY_BSIK,
      WA_BSAK           TYPE TY_BSAK,
      WA_BSAS           TYPE TY_BSAS,
      WA_SAIDA          TYPE TY_SAIDA.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER.

*DATA: VARIANTE         LIKE DISVARIANT.
*DATA: GS_VARIANT_C     TYPE DISVARIANT.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULARIO
*----------------------------------------------------------------------*
" Seleção de Campos (TextField)
SELECTION-SCREEN: BEGIN OF BLOCK B4 WITH FRAME .
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS   FOR WA_ZGL012_AVM-BUKRS OBLIGATORY,       """"Empresa
                S_DT_AV   FOR WA_ZGL012_AVM-DT_AVAL OBLIGATORY.     """"Data de Avaliação

SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_CLIENT RADIOBUTTON GROUP A1,
            P_FORNEC RADIOBUTTON GROUP A1,
            P_CONTA  RADIOBUTTON GROUP A1,
            P_TODOS  RADIOBUTTON GROUP A1.

SELECTION-SCREEN: END OF BLOCK B2.

SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS: S_KUNNR  FOR WA_ZGL012_AVM-KUNNR,                   """"Cliente
                S_LIFNR  FOR WA_ZGL012_AVM-LIFNR,                   """"Fornecedor
                S_HKONT  FOR WA_ZGL012_AVM-HKONT,                   """"Conta
                S_C_VAR  FOR WA_BSIS-HKONT.                         """"Conta Variação
SELECTION-SCREEN: END OF BLOCK B3.
SELECTION-SCREEN: END OF BLOCK B4.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM SELECIONAR_DADOS.
  PERFORM ORGANIZAR_DADOS.
  PERFORM INICIAR_VARIAVEIS.
  PERFORM IMPRIMIR_DADOS.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS .

  SELECT BUKRS DT_AVAL KUNNR LIFNR HKONT BELNR BUZEI BSCHL UMSKZ BUDAT WAERS
         GSBER DMBTR WRBTR DMBE2 KURSF AUGDT  AUGBL  TX_FECH  VLR_ATUALIZADO
         VLR_ACUM_MES_ANT VLR_ACUM_MES_ATU  VLR_VARIACAO  RESULTADO  DT_LCTO
         DOC_LCTO ESTORNO OBJ_KEY CPUDT USNAM ST_REV
     FROM ZGL012_AVM
        INTO TABLE T_ZGL012_AVM
          WHERE BUKRS   IN S_BUKRS
            AND DT_AVAL IN S_DT_AV
            AND (   KUNNR IN S_KUNNR
                 OR LIFNR IN S_LIFNR
                 OR HKONT IN S_HKONT ).

  IF   P_CLIENT EQ 'X'
    OR P_TODOS  EQ 'X'.

*    T_AVM_AUX[] = T_ZGL012_AVM[].

    IF P_CLIENT IS NOT INITIAL.
*      refresh t_avm_aux.

      DELETE T_ZGL012_AVM WHERE LIFNR NE SPACE
                            AND HKONT NE SPACE.
*      T_AVM_AUX[] = T_ZGL012_AVM[].
*      DELETE T_AVM_AUX WHERE KUNNR NOT IN S_KUNNR.
    ENDIF.

    IF S_KUNNR[] IS NOT INITIAL.
      DELETE T_ZGL012_AVM WHERE KUNNR NE SPACE
                            AND KUNNR NOT IN S_KUNNR.
    ENDIF.

    IF T_ZGL012_AVM[] IS NOT INITIAL.
*     Clientes partida em aberto
      SELECT BUKRS BELNR GJAHR KUNNR BLDAT XBLNR BLART ZUONR
       FROM BSID
         INTO TABLE T_BSID
         FOR ALL ENTRIES IN T_ZGL012_AVM
           WHERE BUKRS EQ T_ZGL012_AVM-BUKRS
             AND BELNR EQ T_ZGL012_AVM-BELNR
             AND GJAHR EQ T_ZGL012_AVM-BUDAT(4)
             AND KUNNR EQ T_ZGL012_AVM-KUNNR.

*     Clientes partida compensadas
      SELECT BUKRS BELNR GJAHR KUNNR BLDAT XBLNR BLART ZUONR
        FROM BSAD
          INTO TABLE T_BSAD
          FOR ALL ENTRIES IN T_ZGL012_AVM
            WHERE BUKRS EQ T_ZGL012_AVM-BUKRS
              AND BELNR	EQ T_ZGL012_AVM-BELNR
              AND GJAHR EQ T_ZGL012_AVM-BUDAT(4)
              AND KUNNR EQ T_ZGL012_AVM-KUNNR.

      SELECT KUNNR NAME1
        FROM KNA1
          INTO TABLE T_KNA1
          FOR ALL ENTRIES IN T_ZGL012_AVM
            WHERE KUNNR EQ T_ZGL012_AVM-KUNNR.

    ENDIF.
  ENDIF.

  IF   P_FORNEC EQ 'X'
    OR P_TODOS  EQ 'X'.

    IF P_FORNEC IS NOT INITIAL.
      DELETE T_ZGL012_AVM WHERE KUNNR NE SPACE
                            AND HKONT NE SPACE.
*      T_AVM_AUX[] = T_ZGL012_AVM[].
*      DELETE T_AVM_AUX WHERE LIFNR NOT IN S_LIFNR.
    ENDIF.

    IF S_LIFNR[] IS NOT INITIAL.
      DELETE T_ZGL012_AVM WHERE LIFNR NE SPACE
                            AND LIFNR NOT IN S_LIFNR.
    ENDIF.

    IF  T_ZGL012_AVM[] IS NOT INITIAL.
*     Fornecedores partida em aberto
      SELECT BUKRS BELNR GJAHR LIFNR BLDAT XBLNR BLART ZUONR
        FROM BSIK
          INTO TABLE T_BSIK
          FOR ALL ENTRIES IN T_ZGL012_AVM
            WHERE BUKRS EQ T_ZGL012_AVM-BUKRS
              AND BELNR EQ T_ZGL012_AVM-BELNR
              AND GJAHR EQ T_ZGL012_AVM-BUDAT(4)
              AND LIFNR EQ T_ZGL012_AVM-LIFNR.

*     Fornecedores partida compensadas
      SELECT BUKRS BELNR GJAHR LIFNR BLDAT XBLNR BLART ZUONR
        FROM BSAK
          INTO TABLE T_BSAK
          FOR ALL ENTRIES IN T_ZGL012_AVM
            WHERE BUKRS EQ T_ZGL012_AVM-BUKRS
              AND BELNR EQ T_ZGL012_AVM-BELNR
              AND GJAHR EQ T_ZGL012_AVM-BUDAT(4)
              AND LIFNR EQ T_ZGL012_AVM-LIFNR.

      SELECT LIFNR NAME1
        FROM LFA1
          INTO TABLE T_LFA1
          FOR ALL ENTRIES IN T_ZGL012_AVM
            WHERE LIFNR EQ T_ZGL012_AVM-LIFNR.

    ENDIF.

  ELSEIF P_CONTA EQ 'X'
    OR   P_TODOS  EQ 'X'.

    IF P_CONTA IS NOT INITIAL.
      DELETE T_ZGL012_AVM WHERE LIFNR NE SPACE
                            AND KUNNR NE SPACE.
    ENDIF.

    IF S_HKONT[] IS NOT INITIAL.
      DELETE T_ZGL012_AVM WHERE HKONT NE SPACE
                            AND HKONT NOT IN S_HKONT.
    ENDIF.

    T_ZGL012_AVM_AUX[] = T_ZGL012_AVM[].
    DELETE T_ZGL012_AVM_AUX WHERE LIFNR NE SPACE
                              AND KUNNR NE SPACE.

    IF T_ZGL012_AVM_AUX IS NOT INITIAL.

*     Conta Razão Partidas em Aberto
      SELECT BUKRS BELNR GJAHR HKONT BLDAT XBLNR BLART ZUONR
       FROM BSIS
         INTO TABLE T_BSIS
          FOR ALL ENTRIES IN T_ZGL012_AVM_AUX
            WHERE BUKRS EQ T_ZGL012_AVM_AUX-BUKRS
              AND BELNR EQ T_ZGL012_AVM_AUX-BELNR
              AND GJAHR EQ T_ZGL012_AVM_AUX-BUDAT(4)
              AND HKONT EQ T_ZGL012_AVM_AUX-HKONT.

    ENDIF.

*   Conta Razão partida compensadas
    SELECT BUKRS BELNR GJAHR HKONT BLDAT XBLNR BLART ZUONR
      FROM BSAS
        INTO TABLE T_BSAS
        FOR ALL ENTRIES IN T_ZGL012_AVM_AUX
          WHERE BUKRS EQ T_ZGL012_AVM_AUX-BUKRS
            AND BELNR EQ T_ZGL012_AVM_AUX-BELNR
            AND GJAHR EQ T_ZGL012_AVM_AUX-BUDAT(4)
            AND HKONT EQ T_ZGL012_AVM_AUX-HKONT.

    SELECT SAKNR TXT20
      FROM SKAT
        INTO TABLE T_SKAT
        FOR ALL ENTRIES IN T_ZGL012_AVM_AUX
          WHERE SAKNR EQ T_ZGL012_AVM_AUX-HKONT
            AND SPRAS EQ SY-LANGU.

  ENDIF.

  IF T_ZGL012_AVM[] IS NOT INITIAL.

    SELECT OBJ_KEY BUKRS BELNR GJAHR
      FROM ZIB_CONTABIL_CHV
        INTO TABLE T_CONTABIL_CHV
        FOR ALL ENTRIES IN T_ZGL012_AVM
          WHERE OBJ_KEY EQ T_ZGL012_AVM-OBJ_KEY.

    IF SY-SUBRC IS INITIAL.

*     Conta de Variação
      SELECT BUKRS BELNR GJAHR HKONT BLDAT XBLNR BLART ZUONR
        FROM BSIS
          INTO TABLE T_BSIS_VAR
          FOR ALL ENTRIES IN T_CONTABIL_CHV
            WHERE BUKRS EQ T_CONTABIL_CHV-BUKRS
              AND BELNR EQ T_CONTABIL_CHV-BELNR
              AND GJAHR EQ T_CONTABIL_CHV-GJAHR.

      LOOP AT T_BSIS_VAR INTO WA_BSIS_VAR.

        SHIFT WA_BSIS_VAR-HKONT LEFT DELETING LEADING '0'.

        IF WA_BSIS_VAR-HKONT(1) EQ '1'
          OR WA_BSIS_VAR-HKONT(1) EQ '2'.
          DELETE T_BSIS_VAR.
        ENDIF.

      ENDLOOP.
*      DELETE T_BSIS WHERE HKONT(1) EQ '1'
*                       OR HKONT(1) EQ '2'.

      IF S_C_VAR[] IS NOT INITIAL.
        DELETE T_BSIS_VAR WHERE HKONT NOT IN S_C_VAR.
      ENDIF.

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
FORM ORGANIZAR_DADOS .

  SORT: T_BSID           BY BUKRS BELNR GJAHR KUNNR,
        T_BSAD           BY BUKRS BELNR GJAHR KUNNR,
        T_BSIK           BY BUKRS BELNR GJAHR LIFNR,
        T_BSAK           BY BUKRS BELNR GJAHR LIFNR,
        T_CONTABIL_CHV   BY OBJ_KEY,
        T_BSIS           BY BUKRS BELNR GJAHR,
        T_ZGL012_AVM_AUX BY BUKRS BELNR BUDAT HKONT,
        T_BSAS           BY BUKRS BELNR GJAHR HKONT,
        T_BSIS_VAR       BY BUKRS BELNR GJAHR,
        T_KNA1           BY KUNNR,
        T_LFA1           BY LIFNR,
        T_SKAT           BY SAKNR.



  LOOP AT T_ZGL012_AVM INTO WA_ZGL012_AVM.
    CLEAR: WA_SAIDA, WA_BSID, WA_BSAD, WA_BSIK, WA_BSAK, WA_BSIS, WA_BSAS,
           WA_CONTABIL_CHV, WA_KNA1, WA_LFA1, WA_SKAT, WA_BSIS_VAR.
*   Cliente
    IF WA_ZGL012_AVM-KUNNR IS NOT INITIAL.
      READ TABLE T_BSID INTO WA_BSID
        WITH KEY BUKRS = WA_ZGL012_AVM-BUKRS
                 BELNR = WA_ZGL012_AVM-BELNR
                 GJAHR = WA_ZGL012_AVM-BUDAT(4)
                 KUNNR = WA_ZGL012_AVM-KUNNR
                 BINARY SEARCH.

      IF SY-SUBRC IS INITIAL.
        WA_SAIDA-BLDAT = WA_BSID-BLDAT.
        WA_SAIDA-XBLNR = WA_BSID-XBLNR.
        WA_SAIDA-BLART = WA_BSID-BLART.
        WA_SAIDA-ZUONR = WA_BSID-ZUONR.

      ELSE.
        READ TABLE T_BSAD INTO WA_BSAD
          WITH KEY BUKRS = WA_ZGL012_AVM-BUKRS
                   BELNR = WA_ZGL012_AVM-BELNR
                   GJAHR = WA_ZGL012_AVM-BUDAT(4)
                   KUNNR = WA_ZGL012_AVM-KUNNR
                   BINARY SEARCH.

        IF SY-SUBRC IS INITIAL.
          WA_SAIDA-BLDAT = WA_BSAD-BLDAT.
          WA_SAIDA-XBLNR = WA_BSAD-XBLNR.
          WA_SAIDA-BLART = WA_BSAD-BLART.
          WA_SAIDA-ZUONR = WA_BSAD-ZUONR.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.

      READ TABLE T_KNA1 INTO WA_KNA1
        WITH KEY KUNNR = WA_ZGL012_AVM-KUNNR
                 BINARY SEARCH.

      WA_SAIDA-NAME1  = WA_KNA1-NAME1.
      WA_SAIDA-CODIGO = WA_ZGL012_AVM-KUNNR.
      WA_SAIDA-TIPO   = 'Cliente'.
*   Fornecedor
    ELSEIF WA_ZGL012_AVM-LIFNR IS NOT INITIAL.
      READ TABLE T_BSIK INTO WA_BSIK
        WITH KEY BUKRS = WA_ZGL012_AVM-BUKRS
                 BELNR = WA_ZGL012_AVM-BELNR
                 GJAHR = WA_ZGL012_AVM-BUDAT(4)
                 LIFNR = WA_ZGL012_AVM-LIFNR
                 BINARY SEARCH.

      IF SY-SUBRC IS INITIAL.
        WA_SAIDA-BLDAT = WA_BSIK-BLDAT.
        WA_SAIDA-XBLNR = WA_BSIK-XBLNR.
        WA_SAIDA-BLART = WA_BSIK-BLART.
        WA_SAIDA-ZUONR = WA_BSIK-ZUONR.

      ELSE.

        READ TABLE T_BSAK INTO WA_BSAK
          WITH KEY BUKRS = WA_ZGL012_AVM-BUKRS
                   BELNR = WA_ZGL012_AVM-BELNR
                   GJAHR = WA_ZGL012_AVM-BUDAT(4)
                   LIFNR = WA_ZGL012_AVM-LIFNR
                   BINARY SEARCH.

        IF SY-SUBRC IS INITIAL.
          WA_SAIDA-BLDAT = WA_BSAK-BLDAT.
          WA_SAIDA-XBLNR = WA_BSAK-XBLNR.
          WA_SAIDA-BLART = WA_BSAK-BLART.
          WA_SAIDA-ZUONR = WA_BSAK-ZUONR.

        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.
      READ TABLE T_LFA1 INTO WA_LFA1
        WITH KEY LIFNR = WA_ZGL012_AVM-LIFNR
                 BINARY SEARCH.

      WA_SAIDA-NAME1  = WA_LFA1-NAME1.
      WA_SAIDA-CODIGO = WA_ZGL012_AVM-LIFNR.
      WA_SAIDA-TIPO   = 'Fornecedor'.
*   Conta
    ELSE.
      READ TABLE T_BSIS INTO WA_BSIS
        WITH KEY BUKRS = WA_ZGL012_AVM-BUKRS
                 BELNR = WA_ZGL012_AVM-BELNR
                 GJAHR = WA_ZGL012_AVM-BUDAT(4)
                 HKONT = WA_ZGL012_AVM-HKONT
                 BINARY SEARCH.

      IF SY-SUBRC IS INITIAL.
        WA_SAIDA-BLDAT = WA_BSIS-BLDAT.
        WA_SAIDA-XBLNR = WA_BSIS-XBLNR.
        WA_SAIDA-BLART = WA_BSIS-BLART.
        WA_SAIDA-ZUONR = WA_BSIS-ZUONR.

      ELSE.

        READ TABLE T_BSAS INTO WA_BSAS
          WITH KEY BUKRS = WA_ZGL012_AVM-BUKRS
                   BELNR = WA_ZGL012_AVM-BELNR
                   GJAHR = WA_ZGL012_AVM-BUDAT(4)
                   HKONT = WA_ZGL012_AVM-HKONT
                   BINARY SEARCH.

        IF SY-SUBRC IS INITIAL.
          WA_SAIDA-BLDAT = WA_BSAS-BLDAT.
          WA_SAIDA-XBLNR = WA_BSAS-XBLNR.
          WA_SAIDA-BLART = WA_BSAS-BLART.
          WA_SAIDA-ZUONR = WA_BSAS-ZUONR.

        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.
      READ TABLE T_SKAT INTO WA_SKAT
        WITH KEY SAKNR = WA_ZGL012_AVM-HKONT
                 BINARY SEARCH.

      WA_SAIDA-NAME1  = WA_SKAT-TXT20.
*      WA_SAIDA-CODIGO = WA_ZGL012_AVM-HKONT.
      WA_SAIDA-TIPO   = 'Conta'.
    ENDIF.

    READ TABLE T_CONTABIL_CHV INTO WA_CONTABIL_CHV
      WITH KEY OBJ_KEY = WA_ZGL012_AVM-OBJ_KEY
               BINARY SEARCH.

    IF SY-SUBRC IS INITIAL.
      READ TABLE T_BSIS_VAR INTO WA_BSIS_VAR
        WITH KEY BUKRS = WA_CONTABIL_CHV-BUKRS
                 BELNR = WA_CONTABIL_CHV-BELNR
                 GJAHR = WA_CONTABIL_CHV-GJAHR
                 BINARY SEARCH.

      IF SY-SUBRC IS INITIAL.
        WA_SAIDA-CT_VAR = WA_BSIS_VAR-HKONT.
      ENDIF.

    ENDIF.

    WA_SAIDA-DOC_VARIACAO     = WA_CONTABIL_CHV-BELNR.
    WA_SAIDA-BUKRS            = WA_ZGL012_AVM-BUKRS.
    WA_SAIDA-HKONT            = WA_ZGL012_AVM-HKONT.
    WA_SAIDA-BELNR            = WA_ZGL012_AVM-BELNR.
    WA_SAIDA-BUDAT            = WA_ZGL012_AVM-BUDAT.
    WA_SAIDA-GJAHR            = WA_ZGL012_AVM-DT_AVAL(4).
    WA_SAIDA-BUDAT            = WA_ZGL012_AVM-BUDAT.
    WA_SAIDA-DMBTR            = WA_ZGL012_AVM-DMBTR.
    WA_SAIDA-DMBE2            = WA_ZGL012_AVM-DMBE2.
    WA_SAIDA-WRBTR            = WA_ZGL012_AVM-WRBTR.
    WA_SAIDA-KURSF            = WA_ZGL012_AVM-KURSF.
    WA_SAIDA-WAERS            = WA_ZGL012_AVM-WAERS.
    WA_SAIDA-AUGBL            = WA_ZGL012_AVM-AUGBL.
    WA_SAIDA-AUGDT            = WA_ZGL012_AVM-AUGDT.
    WA_SAIDA-DT_AVAL          = WA_ZGL012_AVM-DT_AVAL.
    WA_SAIDA-VLR_ACUM_MES_ANT = WA_ZGL012_AVM-VLR_ACUM_MES_ANT.
    WA_SAIDA-VLR_VARIACAO     = WA_ZGL012_AVM-VLR_VARIACAO.
    WA_SAIDA-VLR_ACUM_MES_ATU = WA_ZGL012_AVM-VLR_ACUM_MES_ATU.

    APPEND WA_SAIDA TO T_SAIDA.

    CLEAR: WA_SAIDA, WA_BSID, WA_BSAD, WA_BSIK, WA_BSAK, WA_BSIS, WA_BSAS,
           WA_CONTABIL_CHV, WA_KNA1, WA_LFA1, WA_SKAT, WA_BSIS_VAR.

  ENDLOOP.
ENDFORM.                    " ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS .
  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.
  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTAR_LAYOUT." USING 'T_SAIDA'.
*  WL_LAYOUT-BOX_FIELDNAME = 'MARK'.
*  WL_LAYOUT-BOX_TABNAME  = 'T_SAIDA'.

  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
     I_CALLBACK_PROGRAM                = V_REPORT
*     IS_VARIANT                        = GS_VARIANT_C
*    I_CALLBACK_USER_COMMAND           = 'XUSER_COMMAND' "sem 2º click
     IT_FIELDCAT                       = ESTRUTURA[]
     IS_LAYOUT                         = WL_LAYOUT
     I_SAVE                            = 'A'
     IT_EVENTS                         = EVENTS
     IS_PRINT                          = T_PRINT

    TABLES
      T_OUTTAB                          = T_SAIDA.



ENDFORM.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFINIR_EVENTOS.

  PERFORM F_CARREGAR_EVENTOS USING:
                                   SLIS_EV_USER_COMMAND 'XUSER_COMMAND', "para tira duplo click
*                                   SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET',
                                   SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.



ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.

ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT.

  PERFORM MONTAR_ESTRUTURA USING:
        1  'ZGL012_AVM'       'BUKRS'            'T_SAIDA' 'BUKRS'            ' '               ' ' ,      "   Empresa
        2  ' '                ' '                'T_SAIDA' 'TIPO'             'Tipo'            ' ' ,      "   Tipo
        3  'KNA1'             'KUNNR'            'T_SAIDA' 'CODIGO'           'Código'          ' ' ,      "   Código
        4  'KNA1'             'NAME1'            'T_SAIDA' 'NAME1'            'Descrição'       ' ' ,      "   Descrição
        5  'ZGL012_AVM'       'HKONT'            'T_SAIDA' 'HKONT'            ' '               ' ' ,      "   Conta Razão
        6  'ZGL012_AVM'       'BELNR'            'T_SAIDA' 'BELNR'            ' '               ' ' ,      "   Nro. Doc.
        7  'BSIS'             'GJAHR'            'T_SAIDA' 'GJAHR'            'Ano'             ' ' ,      "   Ano
        8  'BSAD'             'XBLNR'            'T_SAIDA' 'XBLNR'            ' '               ' ' ,      "   Referência
        9  'ZGL012_AVM'       'BUDAT'            'T_SAIDA' 'BUDAT'            ' '               ' ' ,      "   Dt.Lcto.
       10  'ZGL012_AVM'       'BLDAT'            'T_SAIDA' 'BLDAT'            'Dt.Docto'        ' ' ,      "   Dt.Docto
       11  'ZGL012_AVM'       'DMBTR'            'T_SAIDA' 'DMBTR'            'Valor R$'        ' ' ,      "   Valor R$
       12  'ZGL012_AVM'       'DMBE2'            'T_SAIDA' 'DMBE2'            'Valor US$'       ' ' ,      "   Valor US$
       13  'ZGL012_AVM'       'WRBTR'            'T_SAIDA' 'WRBTR'            'Valor EUR'       ' ' ,      "   Valor EUR
       14  'ZGL012_AVM'       'KURSF'            'T_SAIDA' 'KURSF'            ' '               ' ' ,      "   Taxa
       15  'ZGL012_AVM'       'WAERS'            'T_SAIDA' 'WAERS'            'Moeda'           ' ' ,      "   Moeda
       16  'ZGL012_AVM'       'BLART'            'T_SAIDA' 'BLART'            'Tp.dcto'         ' ' ,      "   Tp.dcto
       17  'ZGL012_AVM'       'AUGBL'            'T_SAIDA' 'AUGBL'            ' '               ' ' ,      "   Doc.Comp ens.
       18  'ZGL012_AVM'       'AUGDT'            'T_SAIDA' 'AUGDT'            ' '               ' ' ,      "   Dt.Compens.
       19  'ZGL012_AVM'       'DT_AVAL'          'T_SAIDA' 'DT_AVAL'          ' '               ' ' ,      "   Dt.Avaliação
       20  'BSIS'             'HKONT'            'T_SAIDA' 'CT_VAR'           'Cta Variação'    ' ' ,      "   Cta Variação
       21  'ZIB_CONTABIL_CHV' 'BELNR'            'T_SAIDA' 'DOC_VARIACAO'     'Doc Variação'    ' ' ,      "   Doc Variação
       22  'ZGL012_AVM'       'VLR_ACUM_MES_ANT' 'T_SAIDA' 'VLR_ACUM_MES_ANT' 'Vlr.Var.Mês Ant' ' ' ,      "   Vlr.Var.Mês Ant
       23  'ZGL012_AVM'       'VLR_VARIACAO'     'T_SAIDA' 'VLR_VARIACAO'     'Vlr.Var.Mês'     ' ' ,      "   Vlr.Var.Mês
       24  'ZGL012_AVM'       'VLR_ACUM_MES_ATU' 'T_SAIDA' 'VLR_ACUM_MES_ATU' 'Vlr.Var.Acum.'   ' ' ,      "   Vlr.Var.Acum.
       25  'BSAD'             'ZUONR'            'T_SAIDA' 'ZUONR'            ' '               ' ' .      "   Atribuição

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN).

  CLEAR: WA_ESTRUTURA.


  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.

  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.

  IF P_FIELD EQ 'BELNR'
    OR P_FIELD EQ 'DOC_VARIACAO'.
    WA_ESTRUTURA-HOTSPOT = 'X'.
  ENDIF.

  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA
*---------------------------------------------------------------------*
*       FORM XUSER_COMMAND                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XUSER_COMMAND USING UCOMM    LIKE SY-UCOMM
                         SELFIELD TYPE KKBLO_SELFIELD..     "#EC CALLED

  DATA: VL_FORMNAME           TYPE TDSFNAME,
        VL_NAME               TYPE RS38L_FNAM,
        LS_CONTROL            TYPE SSFCTRLOP,
        LS_OPTIONS            TYPE SSFCOMPOP,
        JOB_OUTPUT_INFO       TYPE SSFCRESCL,
        JOB_OUTPUT_OPTIONS    TYPE SSFCRESOP,
        WL_DIALOG.


  CASE SY-UCOMM.
      IF UCOMM EQ '&IC1'.
        IF SELFIELD-FIELDNAME EQ 'BELNR'.
          READ TABLE T_SAIDA INTO WA_SAIDA INDEX SELFIELD-TABINDEX.
*    Set parameter ID for transaction screen field
          SET PARAMETER ID 'BLN' FIELD WA_SAIDA-BELNR.
          SET PARAMETER ID 'GJR' FIELD WA_SAIDA-BUDAT(4).
          SET PARAMETER ID 'BUK' FIELD WA_SAIDA-BUKRS.
*    execute transaction FB03, and skip initial data entry screen
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

        ELSEIF SELFIELD-FIELDNAME EQ 'DOC_VARIACAO'.
          READ TABLE T_SAIDA INTO WA_SAIDA INDEX SELFIELD-TABINDEX.
*    Set parameter ID for transaction screen field
          SET PARAMETER ID 'BLN' FIELD WA_SAIDA-DOC_VARIACAO.
          SET PARAMETER ID 'GJR' FIELD WA_SAIDA-BUDAT(4).
          SET PARAMETER ID 'BUK' FIELD WA_SAIDA-BUKRS.
*    execute transaction FB03, and skip initial data entry screen
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDFORM. "XUSER_COMMAND
*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP
      I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIAR_VARIAVEIS.

  V_REPORT = SY-REPID.

  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-004.

ENDFORM.                    " INICIAR_VARIAVES
