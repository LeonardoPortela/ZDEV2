************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 29.08.2014                                          *
* Objetivo    ...: Interface para Intrução de algodão                  *
* Transação   ...:                                                     *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 29.08.2014   Camila Brand          Criação                           *
************************************************************************
REPORT  ZSDR0044
NO STANDARD PAGE HEADING    "Não exibe cabeçalho standard
LINE-SIZE 076               "Comprimento da Linha
LINE-COUNT 65.              "Número de Linhas

*----------------------------------------------------------------------*
* Declaração Geral
*----------------------------------------------------------------------*
TABLES: ZSDT0045, ZSDT0083, ZSDT0092 .

*&--------------------------------------------------------------------&*
*&  ESTRUTURA
*&--------------------------------------------------------------------&*
DATA: WA_RETURN LIKE ZMME_RETURN_ORDEM_SERVICO,
      IT_RETURN LIKE STANDARD TABLE OF WA_RETURN.

*&---------------------------------------------------------------------*
*& Tipos
*&---------------------------------------------------------------------*
TYPES:
*      BEGIN OF TY_SAIDA,
*        ZSEQ_INST         TYPE   ZSDT0092-ZSEQ_INST,
*        OBJEK             TYPE   ZSDT0092-OBJEK,
*        BUKRS             TYPE   ZSDT0092-BUKRS,
*        WERKS             TYPE   ZSDT0092-WERKS,
*        INSTRUCAO         TYPE   ZSDT0092-INSTRUCAO,
*        DATA_INSTR        TYPE   ZSDT0092-DATA_INSTR,
*        CONTRATO          TYPE   ZSDT0092-CONTRATO,
*        DATA_RETIRADA     TYPE   ZSDT0092-DATA_RETIRADA,
*        DEADLINE_DRAFT    TYPE   ZSDT0092-DEADLINE_DRAFT,
*        DEADLINE_DOCUMEN  TYPE   ZSDT0092-DEADLINE_DOCUMEN,
*        PORTO_EMBARQUE    TYPE   ZSDT0092-PORTO_EMBARQUE,
*        DATA_PORTO        TYPE   ZSDT0092-DATA_PORTO,
*        SAFRA             TYPE   ZSDT0092-SAFRA,
*        STATUS            TYPE   ZSDT0092-STATUS,
*        DATA_CRIACAO      TYPE   ZSDT0092-DATA_CRIACAO,
*        USUARIO           TYPE   ZSDT0092-USUARIO,
*        QUANTIDADE        TYPE   ZSDT0092-QUANTIDADE,
*        VOLEH             TYPE   ZSDT0092-VOLEH,
*        DT_MOD            TYPE   ZSDT0092-DT_MOD,
*        HR_MOD            TYPE   ZSDT0092-HR_MOD,
*        TP_REG            TYPE   ZSDT0092-TP_REG,
*        RG_ATUALIZADO     TYPE   ZSDT0092-RG_ATUALIZADO,
*     END   OF TY_SAIDA,

    BEGIN OF TY_ZSDT0083,
        NRO_SOL_OV   TYPE   ZSDT0083-NRO_SOL_OV,
        ID_HISTORICO TYPE   ZSDT0083-ID_HISTORICO,
        LINHA        TYPE   ZSDT0083-LINHA,
        AREA         TYPE   ZSDT0083-AREA,
        CAMPO        TYPE   ZSDT0083-CAMPO,
        NEW_VALUE    TYPE   ZSDT0083-NEW_VALUE,
        OLD_VALUE    TYPE   ZSDT0083-OLD_VALUE,
        USNAM        TYPE   ZSDT0083-USNAM,
        DATA_ATUAL   TYPE   ZSDT0083-DATA_ATUAL,
        HORA_ATUAL   TYPE   ZSDT0083-HORA_ATUAL,
        OBJEK        TYPE   ZSDT0092-OBJEK,
    END  OF TY_ZSDT0083,

    BEGIN OF TY_ZSDT0045,
        OBJEK             TYPE   ZSDT0083-NRO_SOL_OV,
     END   OF TY_ZSDT0045,

    BEGIN OF TY_ZSDT0066,
      NRO_SOL_OV  TYPE ZSDT0066-NRO_SOL_OV ,
      INSTRUCAO   TYPE ZSDT0066-INSTRUCAO  ,
      WERKS       TYPE ZSDT0066-WERKS      ,
      CHARG       TYPE ZSDT0066-CHARG      ,
      POSNR       TYPE ZSDT0066-POSNR      ,
      ZMENG       TYPE ZSDT0066-ZMENG      ,
    END   OF TY_ZSDT0066.

*&---------------------------------------------------------------------*
*& Tabelas Internas
*&---------------------------------------------------------------------*

DATA : IT_SAIDA             TYPE TABLE OF ZSDT0092,
       IT_ZSDT0045          TYPE TABLE OF ZSDT0045,
       IT_ZSDT0045_AUX      TYPE TABLE OF TY_ZSDT0045,
       IT_ZSDT0083          TYPE TABLE OF TY_ZSDT0083,
       IT_ZSDT0083_AUX      TYPE TABLE OF ZSDT0083,
       IT_ZSDT0066          TYPE TABLE OF TY_ZSDT0066,
       IT_ZSDT0092          TYPE TABLE OF ZSDT0092,
       IT_ZSDT0092_AUX      TYPE TABLE OF ZSDT0092.



*&---------------------------------------------------------------------*
*& WORKAREAS
*&---------------------------------------------------------------------*

DATA :  WA_SAIDA           TYPE ZSDT0092,
        WA_ZSDT0045        TYPE ZSDT0045,
        WA_ZSDT0083        TYPE TY_ZSDT0083,
        WA_ZSDT0083_AUX    TYPE ZSDT0083,
        WA_ZSDT0066        TYPE TY_ZSDT0066,
        WA_ZSDT0092        TYPE ZSDT0092,
        WA_ZSDT0092_AUX    TYPE ZSDT0092.

*&---------------------------------------------------------------------*
*& Ranges
*&---------------------------------------------------------------------*
RANGES: RG_DATE       FOR ZSDT0083-DATA_ATUAL,
        RG_NRO_SOL_OV FOR ZSDT0083-NRO_SOL_OV,
        RG_OBJEK      FOR ZSDT0045-OBJEK.


*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM SELECIONAR_DADOS.
  PERFORM ORGANIZACAO_DADOS.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS .

  REFRESH: RG_DATE,RG_NRO_SOL_OV,RG_OBJEK.


  RG_DATE-SIGN     = 'I'.
  RG_DATE-OPTION   = 'BT'.
  RG_DATE-LOW      = ( SY-DATUM - 5 ).
  RG_DATE-HIGH    = SY-DATUM.
  APPEND  RG_DATE.


  SELECT *
  INTO TABLE IT_ZSDT0045
  FROM ZSDT0045.

  SELECT *
      FROM ZSDT0092
      INTO TABLE IT_ZSDT0092
      FOR ALL ENTRIES IN IT_ZSDT0045
        WHERE ZSEQ_INST EQ IT_ZSDT0045-ZSEQ_INST
          AND OBJEK     EQ IT_ZSDT0045-OBJEK .



  RG_NRO_SOL_OV-SIGN = 'I'.
  RG_NRO_SOL_OV-OPTION = 'EQ'.
  LOOP AT  IT_ZSDT0045 INTO WA_ZSDT0045.
    MOVE WA_ZSDT0045-OBJEK TO RG_NRO_SOL_OV-LOW.
    APPEND RG_NRO_SOL_OV.
    CLEAR WA_ZSDT0045.
  ENDLOOP.

  SELECT *
    INTO TABLE IT_ZSDT0083_AUX
    FROM ZSDT0083
    WHERE CAMPO      EQ 'QUANTIDADE'
      AND AREA       EQ 'instrução'
      AND NRO_SOL_OV IN  RG_NRO_SOL_OV.
  "LINHA   EQ IT_ZSDT0045-ZSEQ_INST

  "Formação de Lote
  SELECT NRO_SOL_OV INSTRUCAO WERKS CHARG POSNR ZMENG
    INTO TABLE IT_ZSDT0066
    FROM ZSDT0066
   WHERE NRO_SOL_OV IN  RG_NRO_SOL_OV
   "GROUP BY NRO_SOL_OV INSTRUCAO WERKS CHARG
    .

  LOOP AT IT_ZSDT0083_AUX  INTO WA_ZSDT0083_AUX.

    WA_ZSDT0083-NRO_SOL_OV     = WA_ZSDT0083_AUX-NRO_SOL_OV.
    WA_ZSDT0083-ID_HISTORICO   = WA_ZSDT0083_AUX-ID_HISTORICO.
    WA_ZSDT0083-LINHA          = WA_ZSDT0083_AUX-LINHA.
    WA_ZSDT0083-AREA           = WA_ZSDT0083_AUX-AREA.
    WA_ZSDT0083-CAMPO          = WA_ZSDT0083_AUX-CAMPO.
    WA_ZSDT0083-NEW_VALUE      = WA_ZSDT0083_AUX-NEW_VALUE.
    WA_ZSDT0083-OLD_VALUE      = WA_ZSDT0083_AUX-OLD_VALUE.
    WA_ZSDT0083-USNAM          = WA_ZSDT0083_AUX-USNAM.

    IF WA_ZSDT0083_AUX-DATA_ATUAL = ''.
      WA_ZSDT0083-DATA_ATUAL = '00000000'.
    ELSE.
      WA_ZSDT0083-DATA_ATUAL     = WA_ZSDT0083_AUX-DATA_ATUAL.
    ENDIF.

    IF   WA_ZSDT0083_AUX-HORA_ATUAL = ''.
      WA_ZSDT0083-HORA_ATUAL = '000000'.
    ELSE.
      WA_ZSDT0083-HORA_ATUAL     = WA_ZSDT0083_AUX-HORA_ATUAL.
    ENDIF.
    MOVE WA_ZSDT0083-NRO_SOL_OV TO WA_ZSDT0083-OBJEK .

    APPEND WA_ZSDT0083 TO IT_ZSDT0083.

  ENDLOOP.

  SELECT *
  FROM ZSDT0092
  INTO TABLE IT_ZSDT0092_AUX
  FOR ALL ENTRIES IN IT_ZSDT0083
    WHERE OBJEK   EQ IT_ZSDT0083-OBJEK
    AND DT_MOD    EQ IT_ZSDT0083-DATA_ATUAL
    AND HR_MOD    EQ IT_ZSDT0083-HORA_ATUAL
    AND USUARIO   EQ IT_ZSDT0083-USNAM
    AND TP_REG    EQ 'A'.



ENDFORM.                    "SELECIONAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZACAO_DADOS .


  DATA: VG_OBJEK     TYPE ZSDT0092-OBJEK,
        VG_NR_SOL_OV TYPE ZSDT0066-NRO_SOL_OV.

  SORT: IT_ZSDT0092     BY ZSEQ_INST OBJEK POSNR,
        IT_ZSDT0092_AUX BY OBJEK DT_MOD HR_MOD USUARIO,
        IT_ZSDT0066     BY NRO_SOL_OV INSTRUCAO WERKS CHARG POSNR.



  LOOP AT  IT_ZSDT0045 INTO WA_ZSDT0045.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_ZSDT0045-OBJEK
      IMPORTING
        OUTPUT = VG_NR_SOL_OV.

    LOOP AT IT_ZSDT0066 INTO WA_ZSDT0066 WHERE NRO_SOL_OV = VG_NR_SOL_OV AND INSTRUCAO = WA_ZSDT0045-INSTRUCAO AND WERKS = WA_ZSDT0045-WERKS AND CHARG = WA_ZSDT0045-SAFRA .
*      IF SY-SUBRC IS NOT INITIAL.
*        CONTINUE.
*      ENDIF.

      READ TABLE IT_ZSDT0092 INTO WA_ZSDT0092  WITH KEY ZSEQ_INST = WA_ZSDT0045-ZSEQ_INST  OBJEK =  WA_ZSDT0045-OBJEK POSNR = WA_ZSDT0066-POSNR BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        CONTINUE.
      ENDIF.

      WA_SAIDA-QUANTIDADE       = WA_ZSDT0066-ZMENG.
      WA_SAIDA-POSNR            = WA_ZSDT0066-POSNR.

      WA_SAIDA-ZSEQ_INST        = WA_ZSDT0045-ZSEQ_INST.
      WA_SAIDA-OBJEK            = WA_ZSDT0045-OBJEK.
      WA_SAIDA-BUKRS            = WA_ZSDT0045-BUKRS.
      WA_SAIDA-WERKS            = WA_ZSDT0045-WERKS.
      WA_SAIDA-INSTRUCAO        = WA_ZSDT0045-INSTRUCAO.
      WA_SAIDA-DATA_INSTR       = WA_ZSDT0045-DATA_INSTR.
      WA_SAIDA-CONTRATO         = WA_ZSDT0045-CONTRATO.
      WA_SAIDA-DATA_RETIRADA    = WA_ZSDT0045-DATA_RETIRADA.
      WA_SAIDA-DEADLINE_DRAFT   = WA_ZSDT0045-DEADLINE_DRAFT.
      WA_SAIDA-DEADLINE_DOCUMEN = WA_ZSDT0045-DEADLINE_DOCUMEN.
      WA_SAIDA-PORTO_EMBARQUE   = WA_ZSDT0045-PORTO_EMBARQUE.
      WA_SAIDA-DATA_PORTO       = WA_ZSDT0045-DATA_PORTO.
      WA_SAIDA-SAFRA            = WA_ZSDT0045-SAFRA.
      WA_SAIDA-STATUS           = WA_ZSDT0045-STATUS.
      WA_SAIDA-DATA_CRIACAO     = WA_ZSDT0045-DATA_CRIACAO.
      WA_SAIDA-USUARIO          = WA_ZSDT0045-USUARIO.
      WA_SAIDA-VOLEH            = WA_ZSDT0045-VOLEH.
      WA_SAIDA-DT_MOD           = '00000000'.
      WA_SAIDA-HR_MOD           = '000000'.
      WA_SAIDA-TP_REG           = 'I'.
      WA_SAIDA-RG_ATUALIZADO    = '0'.

      APPEND WA_SAIDA TO IT_SAIDA.
      MODIFY ZSDT0092 FROM WA_SAIDA.
    ENDLOOP.
  ENDLOOP.

  LOOP AT  IT_ZSDT0083 INTO WA_ZSDT0083.


*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        INPUT  = WA_ZSDT0083-NRO_SOL_OV
*      IMPORTING
*        OUTPUT = VG_OBJEK.

    READ TABLE IT_ZSDT0092_AUX INTO WA_ZSDT0092_AUX  WITH KEY OBJEK = WA_ZSDT0083-OBJEK DT_MOD = WA_ZSDT0083-DATA_ATUAL
    HR_MOD = WA_ZSDT0083-HORA_ATUAL USUARIO = WA_ZSDT0083-USNAM BINARY SEARCH.


    IF SY-SUBRC IS INITIAL.
      CONTINUE.

    ENDIF.


    WA_SAIDA-ZSEQ_INST        =     WA_ZSDT0083-LINHA.
    WA_SAIDA-OBJEK            =     WA_ZSDT0083-NRO_SOL_OV.
    WA_SAIDA-QUANTIDADE       =     WA_ZSDT0083-NEW_VALUE.
    IF WA_ZSDT0083-DATA_ATUAL =  ''.
      WA_SAIDA-DT_MOD         =     '00000000'.
      WA_SAIDA-HR_MOD         =     '000000'.
    ELSE.
      WA_SAIDA-DT_MOD         =     WA_ZSDT0083-DATA_ATUAL.
      WA_SAIDA-HR_MOD         =     WA_ZSDT0083-HORA_ATUAL.
    ENDIF.


    WA_SAIDA-TP_REG           =     'A'.
    WA_SAIDA-RG_ATUALIZADO    =     '0'.
    WA_SAIDA-USUARIO          =    WA_ZSDT0083-USNAM.

    APPEND WA_SAIDA TO IT_SAIDA.
    MODIFY ZSDT0092 FROM WA_SAIDA.

  ENDLOOP.

  IF NOT IT_SAIDA[] IS INITIAL.
    PERFORM F_RFC_ENVIO.
  ENDIF.

ENDFORM.                    "ORGANIZACAO_DADOS

*&---------------------------------------------------------------------*
*&      Form  f_rfc_despesas
*&---------------------------------------------------------------------*
FORM F_RFC_ENVIO .

  DATA: QTD TYPE SY-TABIX.

  CALL FUNCTION 'Z_SD_OUTBOUND_INST_ALGODAO' IN BACKGROUND TASK
    DESTINATION 'XI_INST_ALGODAO' " SM59
    TABLES
      RETURN_SUCESS = IT_SAIDA[].

  COMMIT WORK.

  CLEAR: QTD.
  DESCRIBE TABLE IT_SAIDA LINES QTD.


  IF SY-SUBRC EQ 0.
    MESSAGE S000(Z01) WITH QTD 'Registro(s) Enviados com Sucesso. '.
  ENDIF.



ENDFORM.                    "F_RFC_ENVIO
