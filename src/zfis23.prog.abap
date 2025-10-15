*&---------------------------------------------------------------------*
*& Report  ZFIS23
*&
*&---------------------------------------------------------------------*
*&TITULO: Relatório Comparativo Fiscal Compra X Exportação
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 20.05.2013
*&---------------------------------------------------------------------*


REPORT  ZFIS23.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ZNOM_PROGRAMACAO, J_1BNFDOC.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

TYPES:
  BEGIN OF TY_ZDOC_NF_PRODUTOR,
    DOCNUM_PROD TYPE ZDOC_NF_PRODUTOR-DOCNUM_PROD,
    VBELN       TYPE ZDOC_NF_PRODUTOR-VBELN,
    MENGE       TYPE ZDOC_NF_PRODUTOR-MENGE,
  END OF TY_ZDOC_NF_PRODUTOR,

  BEGIN OF TY_ZNOM_PROGRAMACAO,
    ID_EMPRESA       TYPE ZNOM_PROGRAMACAO-ID_EMPRESA,
    ID_FILIAL        TYPE ZNOM_PROGRAMACAO-ID_FILIAL,
    ID_MATERIAL      TYPE ZNOM_PROGRAMACAO-ID_MATERIAL,
    NR_PROGRAMADA    TYPE ZNOM_PROGRAMACAO-NR_PROGRAMADA,
    ID_NOMEACAO_TRAN TYPE ZNOM_PROGRAMACAO-ID_NOMEACAO_TRAN,
  END OF TY_ZNOM_PROGRAMACAO,

  BEGIN OF TY_ZNOM_REME_NOTAS,
    ID_NOMEACAO_TRAN TYPE ZNOM_REME_NOTAS-ID_NOMEACAO_TRAN,
    ID_FILIAL        TYPE ZNOM_REME_NOTAS-ID_FILIAL,
    ID_MATERIAL      TYPE ZNOM_REME_NOTAS-ID_MATERIAL,
    ID_REMETENTE     TYPE ZNOM_REME_NOTAS-ID_REMETENTE,
    DOCNUM           TYPE ZNOM_REME_NOTAS-DOCNUM,
    ITMNUM           TYPE ZNOM_REME_NOTAS-ITMNUM,
    NR_QUANTIDADE    TYPE ZNOM_REME_NOTAS-NR_QUANTIDADE,
  END OF TY_ZNOM_REME_NOTAS,

  BEGIN OF TY_ZDOC_EXP,
    ID_NOMEACAO_TRAN TYPE ZDOC_EXP-ID_NOMEACAO_TRAN,
    ID_DOC_EXP       TYPE ZDOC_EXP-ID_DOC_EXP,
    VBELN            TYPE ZDOC_EXP-VBELN,
    ID_REGISTRO_EXPO TYPE ZDOC_EXP-ID_REGISTRO_EXPO,
    NR_REGISTRO_EXPO TYPE ZDOC_EXP-NR_REGISTRO_EXPO,
    ID_DUE           TYPE ZDOC_EXP-ID_DUE,
    NUMERO_DUE       TYPE ZDOC_EXP-NUMERO_DUE,
  END OF TY_ZDOC_EXP,

  BEGIN OF TY_J_1BNFDOC,
    DOCNUM TYPE J_1BNFDOC-DOCNUM,
    PSTDAT TYPE J_1BNFDOC-PSTDAT,
    PARID  TYPE J_1BNFDOC-PARID,
    NFENUM TYPE J_1BNFDOC-NFENUM,
    BRANCH TYPE J_1BNFDOC-BRANCH,
  END OF TY_J_1BNFDOC,

  BEGIN OF TY_J_1BNFLIN,
    DOCNUM TYPE J_1BNFLIN-DOCNUM,
    MATNR  TYPE J_1BNFLIN-MATNR,
    MAKTX  TYPE J_1BNFLIN-MAKTX,
    CFOP   TYPE J_1BNFLIN-CFOP,
    NBM    TYPE J_1BNFLIN-NBM,
    NETWR  TYPE J_1BNFLIN-NETWR,
    MENGE  TYPE J_1BNFLIN-MENGE,
    MEINS  TYPE J_1BNFLIN-MEINS,
  END OF TY_J_1BNFLIN,

  BEGIN OF TY_LFA1,
    LIFNR TYPE LFA1-LIFNR,
    NAME1 TYPE LFA1-NAME1,
    STCD1 TYPE LFA1-STCD1,
  END OF TY_LFA1,

  BEGIN OF TY_VBFA,
    VBELN TYPE VBFA-VBELN,
    VBELV TYPE VBFA-VBELV,
    ERDAT TYPE VBFA-ERDAT,
  END OF TY_VBFA,

  BEGIN OF TY_VBRP,
    VGBEL  TYPE VBRP-VGBEL,
    VBELN  TYPE VBRP-VBELN,
    REFKEY TYPE J_1BNFLIN-REFKEY,
  END OF TY_VBRP,

  BEGIN OF TY_J_1BNFLIN_2,
    REFKEY TYPE J_1BNFLIN-REFKEY,
    CFOP   TYPE J_1BNFLIN-CFOP,
    MENGE  TYPE J_1BNFLIN-MENGE,
    NETWR  TYPE J_1BNFLIN-NETWR,
  END OF TY_J_1BNFLIN_2,


  BEGIN OF TY_SAIDA,
    DOCNUM           TYPE J_1BNFDOC-DOCNUM,
    NR_QUANTIDADE    TYPE ZNOM_REME_NOTAS-NR_QUANTIDADE,
    NETWR            TYPE J_1BNFLIN-NETWR,
    STCD1            TYPE LFA1-STCD1,
    LIFNR            TYPE LFA1-LIFNR,
    NAME1_P          TYPE LFA1-NAME1,
    MATNR            TYPE J_1BNFLIN-MATNR,
    MAKTX            TYPE J_1BNFLIN-MAKTX,
    NBM              TYPE J_1BNFLIN-NBM,
    CFOP             TYPE J_1BNFLIN-CFOP,
    VBELN            TYPE ZDOC_EXP-VBELN,
    VBELV            TYPE VBFA-VBELV,
    MENGE            TYPE J_1BNFLIN-MENGE,
    XV_VALOR         TYPE J_1BNFLIN-NETWR,
    XV_CFOP          TYPE J_1BNFLIN-CFOP,
    NR_REGISTRO_EXPO TYPE ZDOC_EXP-NR_REGISTRO_EXPO,
    NUMERO_DUE       TYPE ZDOC_EXP-NUMERO_DUE,
    ID_NOMEACAO_TRAN TYPE ZNOM_REME_NOTAS-ID_NOMEACAO_TRAN,
    PSTDAT           TYPE J_1BNFDOC-PSTDAT,
    ERDAT            TYPE VBFA-ERDAT,
    BRANCH           TYPE J_1BNFDOC-BRANCH,
  END OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: TI_BDCDATA               TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      T_MESSTAB                TYPE TABLE OF BDCMSGCOLL,
      IT_FCAT                  TYPE TABLE OF TY_ESTRUTURA,

      IT_ZDOC_NF_PRODUTOR      TYPE TABLE OF TY_ZDOC_NF_PRODUTOR,
      IT_ZNOM_PROGRAMACAO      TYPE TABLE OF TY_ZNOM_PROGRAMACAO,
      IT_ZNOM_PROGRAMACAO_AGRP TYPE TABLE OF TY_ZNOM_PROGRAMACAO,
      IT_ZNOM_REME_NOTAS       TYPE TABLE OF TY_ZNOM_REME_NOTAS,
      IT_ZDOC_EXP              TYPE TABLE OF TY_ZDOC_EXP,
      IT_J_1BNFDOC             TYPE TABLE OF TY_J_1BNFDOC,
      IT_J_1BNFLIN             TYPE TABLE OF TY_J_1BNFLIN,
      IT_LFA1                  TYPE TABLE OF TY_LFA1,
      IT_VBFA                  TYPE TABLE OF TY_VBFA,
      IT_VBRP                  TYPE TABLE OF TY_VBRP,
      IT_J_1BNFLIN_2           TYPE TABLE OF TY_J_1BNFLIN_2,

      IT_SAIDA                 TYPE TABLE OF TY_SAIDA,
      IT_COLOR                 TYPE TABLE OF LVC_S_SCOL,
      TG_MSG_RET               TYPE TABLE OF ZFIWRS0002 WITH HEADER LINE.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  WA_CONT             TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  WA_ALV              TYPE REF TO CL_GUI_ALV_GRID,
  WA_BDCDATA          LIKE LINE OF TI_BDCDATA,

  WA_ZDOC_NF_PRODUTOR TYPE TY_ZDOC_NF_PRODUTOR,
  WA_ZNOM_PROGRAMACAO TYPE TY_ZNOM_PROGRAMACAO,
  WA_ZNOM_REME_NOTAS  TYPE TY_ZNOM_REME_NOTAS,
  WA_ZDOC_EXP         TYPE TY_ZDOC_EXP,
  WA_J_1BNFDOC        TYPE TY_J_1BNFDOC,
  WA_J_1BNFLIN        TYPE TY_J_1BNFLIN,
  WA_LFA1             TYPE TY_LFA1,
  WA_VBFA             TYPE TY_VBFA,
  WA_VBRP             TYPE TY_VBRP,
  WA_J_1BNFLIN_2      TYPE TY_J_1BNFLIN_2,
  WA_SAIDA            TYPE TY_SAIDA.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
  S_VARIANT       TYPE DISVARIANT           , " Tabela Estrutura co
  T_TOP           TYPE SLIS_T_LISTHEADER,
  XS_EVENTS       TYPE SLIS_ALV_EVENT,
  EVENTS          TYPE SLIS_T_EVENT,
  GD_LAYOUT       TYPE SLIS_LAYOUT_ALV,
  T_PRINT         TYPE SLIS_PRINT_ALV,
  V_REPORT        LIKE SY-REPID,
  T_SORT          TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
  IT_SETLEAF      LIKE TABLE OF SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
  VG_I            TYPE I,
  WG_MENSAGEM(30).

DATA: OK-CODE          TYPE SY-UCOMM,
      VNUM(10)         TYPE C,
      VSEQ(10)         TYPE P,
      WL_ERRO(1),
      WG_DOCUMENTO(10),
      VSTATUS(1),
      INDROW           TYPE LVC_T_ROW,
      W_IND            TYPE LVC_T_ROW WITH HEADER LINE,
      W_CONT           TYPE I,
      W_CONTC(5),
      W_MENSAGEM(50),
      W_FLAG(1)        VALUE ''.

************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
DATA: EDITCONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      EDITOR          TYPE REF TO CL_GUI_TEXTEDIT,
      CL_CONTAINER_95 TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      CL_CONTAINER_05 TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      OBJ_DYNDOC_ID   TYPE REF TO CL_DD_DOCUMENT,
      CL_GRID         TYPE REF TO CL_GUI_ALV_GRID,
      WA_STABLE       TYPE LVC_S_STBL,
      WA_AFIELD       TYPE LVC_S_FCAT,
      IT_FIELDCAT     TYPE LVC_T_FCAT,
      W_FIELDCAT      TYPE LVC_S_FCAT,
      I_SORT          TYPE LVC_T_SORT,
      WA_LAYOUT       TYPE LVC_S_LAYO,
      IS_STABLE       TYPE LVC_S_STBL VALUE 'XX',
      WG_REPNAME      LIKE SY-REPID,
      WG_X_VARIANT    LIKE DISVARIANT,
      WG_EXIT(1)      TYPE C,
      WG_SAVE(1)      TYPE C,
      WG_VARIANT      LIKE DISVARIANT.


*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: C_0               TYPE C VALUE '0',
           C_1               TYPE C VALUE '1',
           C_2               TYPE C VALUE '2',
           C_B               TYPE C VALUE 'B',
           C_S               TYPE C VALUE 'S',
           C_L               TYPE C VALUE 'L',
           C_X               TYPE C VALUE 'X',
           C_D               TYPE C VALUE 'D',
           C_K               TYPE C VALUE 'K',
           C_W               TYPE C VALUE 'W',
           C_F               TYPE C VALUE 'F',
           C_T               TYPE C VALUE 'T',
           C_I               TYPE C VALUE 'I',
           C_N               TYPE C VALUE 'N',
           C_H               TYPE C VALUE 'H',
           C_AG(2)           TYPE C VALUE 'AG',
           C_NE(2)           TYPE C VALUE 'NE',
           C_01(2)           TYPE C VALUE '01',
           C_30(2)           TYPE C VALUE '30',
           C_40(2)           TYPE C VALUE '40',
           C_50(4)           TYPE C VALUE '0050',
           C_76(2)           TYPE C VALUE '76',
           C_71(2)           TYPE C VALUE '71',
           C_72(2)           TYPE C VALUE '72',
           C_BR(2)           TYPE C VALUE 'BR',
           C_LF(2)           TYPE C VALUE 'LF',
           C_LR(2)           TYPE C VALUE 'LR',
           C_Z1(2)           TYPE C VALUE 'Z1',
           C_ADD(3)          TYPE C VALUE 'ADD',
           C_DEL(3)          TYPE C VALUE 'DEL',
           C_DG1(3)          TYPE C VALUE 'DG1',
           C_DG2(3)          TYPE C VALUE 'DG2',
           C_DUMMY_HEADER(3) TYPE C VALUE '099',
           C_DUMMY_ITENS(3)  TYPE C VALUE '098',
           C_EXIT(4)         TYPE C VALUE 'EXIT',
           C_ROOT(4)         TYPE C VALUE 'ROOT',
           C_MINIMIZAR(4)    TYPE C VALUE '@K2@',
           C_MAXIMIZAR(4)    TYPE C VALUE '@K1@',
           C_BACK(4)         TYPE C VALUE 'BACK',
           C_SAVE(4)         TYPE C VALUE 'SAVE',
           C_DESAT(5)        TYPE C VALUE 'DESAT',
           C_DMBTR(5)        TYPE C VALUE 'DMBTR',
           C_MODIF(5)        TYPE C VALUE 'MODIF',
           C_CANCEL(6)       TYPE C VALUE 'CANCEL',
           C_DELDOC(6)       TYPE C VALUE 'DELDOC',
           C_DCLICK(6)       TYPE C VALUE 'DCLICK',
           C_SEARCH(6)       TYPE C VALUE 'SEARCH',
           C_ATUALI(6)       TYPE C VALUE 'ATUALI',
           C_ADD_MSG(7)      TYPE C VALUE 'ADD_MSG',
           C_DEL_MSG(7)      TYPE C VALUE 'DEL_MSG',
           C_CLOS_MSG(8)     TYPE C VALUE 'CLOS_MSG',
           C_SAVE_MSG(8)     TYPE C VALUE 'SAVE_MSG',
           C_SHOW_MSGRE(10)  TYPE C VALUE 'SHOW_MSGRE'.


*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_BUKRS  FOR ZNOM_PROGRAMACAO-ID_EMPRESA OBLIGATORY  ,
                P_FILIAL FOR ZNOM_PROGRAMACAO-ID_FILIAL              ,
                P_NOMEA  FOR ZNOM_PROGRAMACAO-ID_NOMEACAO_TRAN       ,
                P_PERIO  FOR J_1BNFDOC-PSTDAT OBLIGATORY             .
SELECTION-SCREEN: END OF BLOCK B1.



*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:  F_INICIAR_VARIAVES, " Cabeçalho
            F_SELECIONA_DADOS, " Form seleciona dados
*            F_SAIDA, " Form de saida
            F_IMPRIME_DADOS.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_INICIAR_VARIAVES .
  DATA:
    W_TEXTO1(10),
    W_TEXTO2(10),
    W_TEXTO3(40),
    WL_LINHA(60),
    WL_DATA(10),

    W_EMPRESA_TEXTO(40),
    W_EXER_TEXTO(40),
    W_PER_TEXTO(40),

    EMPRESA             TYPE C LENGTH 50,
    EXERCICIO           TYPE C LENGTH 50,
    PERIODO             TYPE C LENGTH 50.


  V_REPORT = SY-REPID.

  W_TEXTO3 = 'Comparativo Fiscal Compra X Exportação'.
  PERFORM F_CONSTRUIR_CABECALHO USING 'H' W_TEXTO3.

  IF P_BUKRS IS NOT INITIAL.
    IF P_BUKRS-HIGH IS INITIAL.
      CONCATENATE 'Empresa    :' P_BUKRS-LOW
        INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Empresa  :' P_BUKRS-LOW 'à' P_BUKRS-HIGH
      INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_LINHA.
  ENDIF.

  IF P_FILIAL   IS NOT INITIAL.
    IF P_FILIAL-HIGH IS INITIAL.
      CONCATENATE 'Filial  :' P_FILIAL-LOW
       INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Filial  :' P_FILIAL-LOW 'à' P_FILIAL-HIGH
      INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_LINHA.
  ENDIF.

  IF P_NOMEA   IS NOT INITIAL.
    IF P_NOMEA-HIGH IS INITIAL.
      CONCATENATE 'Nomeação  :' P_NOMEA-LOW
       INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Nomeação  :' P_NOMEA-LOW 'à' P_NOMEA-HIGH
      INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_LINHA.
  ENDIF.

  IF P_PERIO IS NOT INITIAL.
    CONCATENATE P_PERIO-LOW+6(2) P_PERIO-LOW+4(2) P_PERIO-LOW+0(4) INTO  WL_DATA SEPARATED BY '.'.
    IF P_PERIO-HIGH IS INITIAL.
      CONCATENATE 'Periodo  :' WL_DATA
      INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Periodo :' WL_DATA  INTO WL_LINHA SEPARATED BY SPACE.
      CONCATENATE P_PERIO-HIGH+6(2) P_PERIO-HIGH+4(2) P_PERIO-HIGH+0(4) INTO  WL_DATA SEPARATED BY '.'.
      CONCATENATE WL_LINHA 'à' WL_DATA  INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_LINHA.
  ENDIF.


ENDFORM.                    " F_INICIAR_VARIAVES

*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TYP        text
*      -->TEXT       text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO    USING TYP TEXT.


  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .

  SELECT ID_EMPRESA ID_FILIAL ID_MATERIAL  NR_PROGRAMADA ID_NOMEACAO_TRAN
    FROM ZNOM_PROGRAMACAO
    INTO TABLE IT_ZNOM_PROGRAMACAO
    WHERE ID_EMPRESA  IN P_BUKRS
    AND   ID_FILIAL   IN P_FILIAL
    AND   ID_NOMEACAO_TRAN IN P_NOMEA.

  SORT IT_ZNOM_PROGRAMACAO BY ID_NOMEACAO_TRAN ID_FILIAL.
  DELETE ADJACENT DUPLICATES FROM IT_ZNOM_PROGRAMACAO COMPARING ID_NOMEACAO_TRAN ID_FILIAL.

  CHECK IT_ZNOM_PROGRAMACAO[] IS NOT INITIAL.

  LOOP AT IT_ZNOM_PROGRAMACAO INTO WA_ZNOM_PROGRAMACAO.

    FREE: IT_ZNOM_REME_NOTAS, IT_J_1BNFDOC, IT_ZDOC_NF_PRODUTOR, IT_ZDOC_EXP,
          IT_J_1BNFLIN, IT_LFA1, IT_VBFA, IT_VBRP, IT_J_1BNFLIN_2.

    SELECT ZNOM_REME_NOTAS~ID_NOMEACAO_TRAN
           ZNOM_REME_NOTAS~ID_FILIAL
           ZNOM_REME_NOTAS~ID_MATERIAL
           ZNOM_REME_NOTAS~ID_REMETENTE
           ZNOM_REME_NOTAS~DOCNUM
           ZNOM_REME_NOTAS~ITMNUM
           ZNOM_REME_NOTAS~NR_QUANTIDADE
      FROM ZNOM_REME_NOTAS
      INNER JOIN J_1BNFDOC ON J_1BNFDOC~DOCNUM = ZNOM_REME_NOTAS~DOCNUM
      INTO TABLE IT_ZNOM_REME_NOTAS
      WHERE ZNOM_REME_NOTAS~ID_NOMEACAO_TRAN  EQ WA_ZNOM_PROGRAMACAO-ID_NOMEACAO_TRAN
        AND ZNOM_REME_NOTAS~ID_FILIAL  EQ WA_ZNOM_PROGRAMACAO-ID_FILIAL
        AND J_1BNFDOC~PSTDAT  IN P_PERIO.

****  SELECT ZNOM_REME_NOTAS~ID_NOMEACAO_TRAN
****         ZNOM_REME_NOTAS~ID_FILIAL
****         ZNOM_REME_NOTAS~ID_MATERIAL
****         ZNOM_REME_NOTAS~ID_REMETENTE
****         ZNOM_REME_NOTAS~DOCNUM
****         ZNOM_REME_NOTAS~ITMNUM
****         ZNOM_REME_NOTAS~NR_QUANTIDADE
****    FROM ZNOM_REME_NOTAS
****    INNER JOIN J_1BNFDOC ON J_1BNFDOC~DOCNUM = ZNOM_REME_NOTAS~DOCNUM
****    INTO TABLE IT_ZNOM_REME_NOTAS
****    FOR ALL ENTRIES IN IT_ZNOM_PROGRAMACAO
****    WHERE ZNOM_REME_NOTAS~ID_NOMEACAO_TRAN  EQ IT_ZNOM_PROGRAMACAO-ID_NOMEACAO_TRAN
****    AND J_1BNFDOC~PSTDAT  IN P_PERIO.

*  SELECT ID_NOMEACAO_TRAN ID_DOC_EXP VBELN ID_REGISTRO_EXPO NR_REGISTRO_EXPO
*    FROM ZDOC_EXP
*    INTO TABLE IT_ZDOC_EXP
*    FOR ALL ENTRIES IN IT_ZNOM_PROGRAMACAO
*    WHERE ID_NOMEACAO_TRAN EQ  IT_ZNOM_PROGRAMACAO-ID_NOMEACAO_TRAN.


    IF IT_ZNOM_REME_NOTAS[] IS NOT INITIAL.
      SELECT  DOCNUM PSTDAT PARID NFENUM BRANCH
        FROM J_1BNFDOC
        INTO TABLE IT_J_1BNFDOC
        FOR ALL ENTRIES IN IT_ZNOM_REME_NOTAS
        WHERE DOCNUM EQ IT_ZNOM_REME_NOTAS-DOCNUM.

      SELECT  DOCNUM_PROD VBELN MENGE
        FROM ZDOC_NF_PRODUTOR
        INTO TABLE IT_ZDOC_NF_PRODUTOR
        FOR ALL ENTRIES IN IT_ZNOM_REME_NOTAS
        WHERE DOCNUM_PROD	=	IT_ZNOM_REME_NOTAS-DOCNUM.

      IF IT_ZDOC_NF_PRODUTOR[] IS NOT INITIAL.
        SELECT ID_NOMEACAO_TRAN ID_DOC_EXP VBELN ID_REGISTRO_EXPO NR_REGISTRO_EXPO ID_DUE NUMERO_DUE
        FROM ZDOC_EXP
        INTO TABLE IT_ZDOC_EXP
        FOR ALL ENTRIES IN IT_ZDOC_NF_PRODUTOR
        WHERE VBELN EQ  IT_ZDOC_NF_PRODUTOR-VBELN
          AND ID_NOMEACAO_TRAN EQ WA_ZNOM_PROGRAMACAO-ID_NOMEACAO_TRAN.

      ENDIF.
    ENDIF.

    IF IT_J_1BNFDOC[] IS NOT INITIAL.
      SELECT  DOCNUM MATNR MAKTX CFOP NBM NETWR MENGE MEINS
        FROM J_1BNFLIN
        INTO TABLE IT_J_1BNFLIN
        FOR ALL ENTRIES IN IT_J_1BNFDOC
        WHERE DOCNUM  EQ  IT_J_1BNFDOC-DOCNUM.

      SELECT  LIFNR NAME1 STCD1
        FROM LFA1
        INTO TABLE IT_LFA1
        FOR ALL ENTRIES IN IT_J_1BNFDOC
        WHERE LIFNR EQ IT_J_1BNFDOC-PARID.
    ENDIF.

    IF IT_ZDOC_EXP[] IS NOT INITIAL.
      SELECT VBELN VBELV ERDAT
        FROM VBFA
        INTO TABLE IT_VBFA
        FOR ALL ENTRIES IN IT_ZDOC_EXP
        WHERE VBELN  EQ IT_ZDOC_EXP-VBELN
        AND VBTYP_N  EQ 'J'
        AND VBTYP_V  EQ 'C'.

      IF IT_VBFA[] IS NOT INITIAL.
        SELECT VGBEL VBELN
        FROM VBRP
        INTO TABLE IT_VBRP
        FOR ALL ENTRIES IN IT_VBFA
        WHERE VGBEL EQ  IT_VBFA-VBELN AND DRAFT = SPACE .

        LOOP AT IT_VBRP INTO WA_VBRP.
          WA_VBRP-REFKEY+0(10) =  WA_VBRP-VBELN.
          MODIFY IT_VBRP FROM WA_VBRP INDEX SY-TABIX TRANSPORTING REFKEY.
        ENDLOOP.

        IF IT_VBRP[] IS NOT INITIAL.
          SELECT REFKEY CFOP MENGE NETWR
            FROM J_1BNFLIN
            INTO TABLE IT_J_1BNFLIN_2
            FOR ALL ENTRIES IN IT_VBRP
            WHERE REFKEY   EQ  IT_VBRP-REFKEY .
        ENDIF.
      ENDIF.
    ENDIF.

    PERFORM F_SAIDA.

  ENDLOOP.


ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA .

  DATA: VLOOP1(1),
        VLOOP2(1).

  DATA VL_UNIT TYPE VBKD-KURRF.

  SORT: IT_ZNOM_REME_NOTAS      BY ID_NOMEACAO_TRAN,
        IT_ZDOC_NF_PRODUTOR     BY DOCNUM_PROD MENGE,
        IT_ZDOC_EXP             BY VBELN ID_NOMEACAO_TRAN,
        IT_J_1BNFDOC            BY DOCNUM PSTDAT,
        IT_J_1BNFLIN            BY DOCNUM,
        IT_LFA1                 BY LIFNR,
        IT_VBFA                 BY VBELN,
        IT_VBRP                 BY VGBEL,
        IT_J_1BNFLIN_2          BY REFKEY.


  "LOOP AT IT_ZNOM_PROGRAMACAO INTO WA_ZNOM_PROGRAMACAO.

  LOOP AT IT_ZNOM_REME_NOTAS INTO WA_ZNOM_REME_NOTAS WHERE ID_NOMEACAO_TRAN = WA_ZNOM_PROGRAMACAO-ID_NOMEACAO_TRAN.

*      WA_SAIDA-NR_QUANTIDADE    = WA_ZNOM_REME_NOTAS-NR_QUANTIDADE.
*      WA_SAIDA-ID_NOMEACAO_TRAN = WA_ZNOM_REME_NOTAS-ID_NOMEACAO_TRAN.

*      READ TABLE IT_ZDOC_NF_PRODUTOR INTO WA_ZDOC_NF_PRODUTOR WITH KEY DOCNUM_PROD  = WA_ZNOM_REME_NOTAS-DOCNUM
*                                                                       MENGE        = WA_ZNOM_REME_NOTAS-NR_QUANTIDADE BINARY SEARCH.
*
*      CHECK SY-SUBRC = 0.
    LOOP AT IT_ZDOC_NF_PRODUTOR INTO WA_ZDOC_NF_PRODUTOR WHERE DOCNUM_PROD  = WA_ZNOM_REME_NOTAS-DOCNUM.

      READ TABLE IT_ZDOC_EXP INTO WA_ZDOC_EXP WITH KEY VBELN            = WA_ZDOC_NF_PRODUTOR-VBELN
                                                       ID_NOMEACAO_TRAN = WA_ZNOM_PROGRAMACAO-ID_NOMEACAO_TRAN BINARY SEARCH.

      WA_SAIDA-ID_NOMEACAO_TRAN = WA_ZNOM_REME_NOTAS-ID_NOMEACAO_TRAN.
      WA_SAIDA-NR_QUANTIDADE    = WA_ZDOC_NF_PRODUTOR-MENGE.

      IF SY-SUBRC = 0.
        WA_SAIDA-NR_REGISTRO_EXPO = WA_ZDOC_EXP-NR_REGISTRO_EXPO.
        WA_SAIDA-NUMERO_DUE       = WA_ZDOC_EXP-NUMERO_DUE.
        WA_SAIDA-VBELN            = WA_ZDOC_EXP-VBELN.

        READ TABLE IT_VBFA INTO WA_VBFA WITH KEY VBELN =  WA_ZDOC_EXP-VBELN BINARY SEARCH.
        IF SY-SUBRC = 0.
          WA_SAIDA-VBELV            = WA_VBFA-VBELV.
          WA_SAIDA-ERDAT            = WA_VBFA-ERDAT.

          READ TABLE IT_VBRP INTO WA_VBRP WITH KEY VGBEL = WA_VBFA-VBELN BINARY SEARCH.
          IF SY-SUBRC = 0.

            READ TABLE IT_J_1BNFLIN_2 INTO WA_J_1BNFLIN_2 WITH KEY REFKEY = WA_VBRP-REFKEY BINARY SEARCH.
            WA_SAIDA-MENGE            = WA_J_1BNFLIN_2-MENGE.
            WA_SAIDA-XV_VALOR         = WA_J_1BNFLIN_2-NETWR.
            WA_SAIDA-XV_CFOP          = WA_J_1BNFLIN_2-CFOP .
          ENDIF.

        ENDIF.

        CLEAR: VLOOP1,VLOOP2.

        LOOP AT IT_J_1BNFDOC INTO WA_J_1BNFDOC WHERE DOCNUM = WA_ZNOM_REME_NOTAS-DOCNUM.

          VLOOP1 = 'X'.
          WA_SAIDA-DOCNUM           = WA_J_1BNFDOC-DOCNUM.
          WA_SAIDA-PSTDAT           = WA_J_1BNFDOC-PSTDAT.
          WA_SAIDA-BRANCH           = WA_J_1BNFDOC-BRANCH.
          READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR    = WA_J_1BNFDOC-PARID BINARY SEARCH.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              INPUT  = WA_LFA1-LIFNR
            IMPORTING
              OUTPUT = WA_SAIDA-LIFNR.

          WA_SAIDA-STCD1            = WA_LFA1-STCD1.
          WA_SAIDA-NAME1_P          = WA_LFA1-NAME1.

          LOOP AT IT_J_1BNFLIN  INTO WA_J_1BNFLIN WHERE DOCNUM = WA_J_1BNFDOC-DOCNUM.
            VLOOP2 = 'X'.
*            WA_SAIDA-NETWR            = WA_J_1BNFLIN-NETWR.
            IF WA_J_1BNFLIN-MENGE EQ WA_ZDOC_NF_PRODUTOR-MENGE .
                WA_SAIDA-NETWR  = WA_J_1BNFLIN-NETWR.
            ELSE.
               VL_UNIT  = WA_J_1BNFLIN-NETWR / WA_J_1BNFLIN-MENGE.
               WA_SAIDA-NETWR = WA_ZDOC_NF_PRODUTOR-MENGE * VL_UNIT.
            ENDIF.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = WA_J_1BNFLIN-MATNR
              IMPORTING
                OUTPUT = WA_SAIDA-MATNR.

            WA_SAIDA-MAKTX            = WA_J_1BNFLIN-MAKTX.
            WA_SAIDA-NBM              = WA_J_1BNFLIN-NBM.
            WA_SAIDA-CFOP             = WA_J_1BNFLIN-CFOP.
            APPEND WA_SAIDA TO IT_SAIDA.
            CLEAR VL_UNIT.
          ENDLOOP.
          IF VLOOP2 = ''.
            APPEND WA_SAIDA TO IT_SAIDA.
            CLEAR: WA_SAIDA-NETWR,
                   WA_SAIDA-MATNR,
                   WA_SAIDA-MAKTX,
                   WA_SAIDA-NBM,
                   WA_SAIDA-CFOP.
          ENDIF.

        ENDLOOP.

        IF VLOOP1 = ''.
          APPEND WA_SAIDA TO IT_SAIDA.
        ENDIF.
      ENDIF.
      CLEAR WA_SAIDA.
    ENDLOOP.
  ENDLOOP.

  "ENDLOOP.

ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPRIME_DADOS .

*  PERFORM F_ALV_FIELDCAT.
*
*  WA_LAYOUT-ZEBRA      = 'X'.
*  WA_LAYOUT-NO_ROWMOVE = 'X'.
*  WA_LAYOUT-NO_ROWINS  = 'X'.
*  WA_LAYOUT-NO_ROWMARK = SPACE.
*  WA_LAYOUT-GRID_TITLE = 'Relatório de Despesas com Vendas'.
*  WA_LAYOUT-SEL_MODE   = 'A'.
*  WA_LAYOUT-CWIDTH_OPT   = 'X'.

  "CALL SCREEN 0100.

  PERFORM F_DEFINIR_EVENTOS.
  PERFORM F_ALV_SORT.
  PERFORM F_ALV.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = V_REPORT
      IS_LAYOUT          = GD_LAYOUT
      "I_CALLBACK_PF_STATUS_SET  = 'SET_PF_STATUS'
      "I_CALLBACK_USER_COMMAND   = 'USER_COMMAND'
      IT_FIELDCAT        = IT_FCAT[]
      IT_SORT            = T_SORT[]
      I_SAVE             = 'X'
      IT_EVENTS          = EVENTS
      IS_PRINT           = T_PRINT
*     IS_VARIANT         = VG_VARIANT
    TABLES
      T_OUTTAB           = IT_SAIDA.
ENDFORM.                    " F_IMPRIME_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_DEFINIR_EVENTOS .
  PERFORM F_CARREGAR_EVENTOS USING:
                                   SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.

ENDFORM.                    " F_DEFINIR_EVENTOS

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_0621   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                      " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_SORT .

ENDFORM.                    " F_ALV_SORT

*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_ALV .

  PERFORM ALV_PREENCHE_CAT USING:
              'ID_NOMEACAO_TRAN'     	'Nro.Nomeação'       '15' ' '     ' '    ' ' ,
              'BRANCH'                'Filial'             '15' ' '     ' '    ' ' ,
              'DOCNUM'                'DOCNUM Compra'      '15' ' '     ' '    ' ' ,
              'PSTDAT'                'Dt.Lcto'            '15' ' '     ' '    ' ' ,
              'NR_QUANTIDADE'         'Qte.Compra'         '15' ' '     ' '    ' ' ,
              'NETWR'                 'Vlr.Compra'         '15' ' '     ' '    ' ' ,
              'STCD1'                 'CNPJ Produtor'      '15' ' '     ' '    ' ' ,
              'LIFNR'                 'Cod. Produtor'      '15' ' '     ' '    ' ' ,
              'NAME1_P'               'Nome Produtor'      '30' ' '     ' '    ' ' ,
              'MATNR'                 'Material'           '15' ' '     ' '    ' ' ,
              'MAKTX'                 'Descr.Material'     '30' ' '     ' '    ' ' ,
              'NBM'                   'NCM Material'       '15' ' '     ' '    ' ' ,
              'CFOP'                  'CFOP Entrada'       '10' ' '     ' '    ' ' ,
              'VBELN'                 'Nro. Remessa Saída' '15' ' '     ' '    ' ' ,
              'ERDAT'                 'Dt. Remessa'        '15' ' '     ' '    ' ' ,
              'VBELV'                 'Nro. O.V. Saída'    '15' ' '     ' '    ' ' ,
              'MENGE'                 'Qte. OV.'           '15' ' '     ' '    ' ' ,
              'XV_VALOR'              'Valor Venda'        '15' ' '     ' '    ' ' ,
              'XV_CFOP'               'XV_CFOP'            '15' ' '     ' '    ' ' ,
              'NR_REGISTRO_EXPO'      'Nro.Reg.Exportação' '15' ' '     ' '    ' ' ,
              'NUMERO_DUE'            'Nro.DU-e'           '15' ' '     ' '    ' ' .


ENDFORM.                    " F_AL
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0649   text
*      -->P_TEXT_003  text
*      -->P_0651   text
*      -->P_0652   text
*      -->P_0653   text
*      -->P_0654   text
*----------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING   P_CAMPO  TYPE C
                               P_DESC   TYPE C
                               P_TAM    TYPE C
                               P_HOT    TYPE C
                               P_ZERO   TYPE C
                               P_SOMA   TYPE C.


  DATA: WL_FCAT TYPE TY_ESTRUTURA.

  WL_FCAT-TABNAME   = 'IT_SAIDA'.
  WL_FCAT-FIELDNAME = P_CAMPO.
  WL_FCAT-SELTEXT_S = P_DESC.
  WL_FCAT-SELTEXT_M = P_DESC.
  WL_FCAT-SELTEXT_L = P_DESC.
  WL_FCAT-HOTSPOT   = P_HOT.
  WL_FCAT-NO_ZERO   = P_ZERO.
  WL_FCAT-OUTPUTLEN = P_TAM.
  WL_FCAT-DO_SUM    = P_SOMA.
  IF P_CAMPO = 'ICON'.
    WL_FCAT-ICON      = 'X'.
  ENDIF.


  APPEND WL_FCAT TO IT_FCAT.
ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT .


  DATA I TYPE I.
  WA_AFIELD-TABNAME     = 'IT_SAIDA'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ID_NOMEACAO_TRAN'.
  WA_AFIELD-SCRTEXT_S = 'Nro.Nomeação'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DOCNUM'.
  WA_AFIELD-SCRTEXT_S = 'DOCNUM Compra'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'PSTDAT'.
  WA_AFIELD-SCRTEXT_S = 'Dt.Lcto'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NR_QUANTIDADE'.
  WA_AFIELD-SCRTEXT_S = 'Qte.Compra'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NETWR'.
  WA_AFIELD-SCRTEXT_S = 'Vlr.Compra'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'STCD1'.
  WA_AFIELD-SCRTEXT_S = 'CNPJ Produtor'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'LIFNR'.
  WA_AFIELD-SCRTEXT_S = 'Cod. Produtor'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NAME1_P'.
  WA_AFIELD-SCRTEXT_S = 'Nome Produtor'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MATNR'.
  WA_AFIELD-SCRTEXT_S = 'Material'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MAKTX'.
  WA_AFIELD-SCRTEXT_S = 'Descr.Material'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NBM'.
  WA_AFIELD-SCRTEXT_S = 'NCM Material'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CFOP'.
  WA_AFIELD-SCRTEXT_S = 'CFOP Entrada'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VBELN'.
  WA_AFIELD-SCRTEXT_S = 'Nro. Remessa Saída'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ERDAT'.
  WA_AFIELD-SCRTEXT_S = 'Dt. Remessa'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VBELV'.
  WA_AFIELD-SCRTEXT_S = 'Nro. O.V. Saída'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MENGE'.
  WA_AFIELD-SCRTEXT_S = 'Qte. OV.'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'XV_VALOR'.
  WA_AFIELD-SCRTEXT_S = 'Valor Venda'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'XV_CFOP'.
  WA_AFIELD-SCRTEXT_S = 'CFOP Saida'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NR_REGISTRO_EXPO'.
  WA_AFIELD-SCRTEXT_S = 'Nro.Reg.Exportação'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NUMERO_DUE'.
  WA_AFIELD-SCRTEXT_S = 'Nro.DU-e'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: FCODE TYPE TABLE OF SY-UCOMM.
  REFRESH: FCODE.

  SET PF-STATUS 'F_SET_PF' EXCLUDING FCODE.
  SET TITLEBAR  'ZFTITLE'.


  IF CL_CONTAINER_95 IS INITIAL.
    CREATE OBJECT CL_CONTAINER_95
      EXPORTING
        SIDE  = '4'
        RATIO = '80'.
  ENDIF.

  IF NOT CL_GRID IS INITIAL.

    PERFORM ZF_ALV_HEADER.
    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CREATE OBJECT OBJ_DYNDOC_ID
      EXPORTING
*       STYLE      =
*       BACKGROUND_COLOR =
*       BDS_STYLESHEET =
        NO_MARGINS = 'X'.

    PERFORM ZF_ALV_HEADER .


    IF EDITCONTAINER IS INITIAL .
      CREATE OBJECT EDITCONTAINER
        EXPORTING
          CONTAINER_NAME = 'HEADER'.
    ENDIF .

    CALL METHOD OBJ_DYNDOC_ID->MERGE_DOCUMENT.

    CALL METHOD OBJ_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = EDITCONTAINER
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.


    CREATE OBJECT CL_GRID
      EXPORTING
        I_PARENT = CL_CONTAINER_95.
*         I_PARENT      = CL_CONTAINER
*         I_APPL_EVENTS = 'X'.

    WG_SAVE = 'X'.

    WA_STABLE-ROW        = C_X.
    CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING        "IS_VARIANT = WG_X_VARIANT
        "I_SAVE = WG_SAVE
        IS_LAYOUT       = WA_LAYOUT
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCAT[]
        IT_SORT         = I_SORT[]
        IT_OUTTAB       = IT_SAIDA[].

  ENDIF.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_ALV_HEADER .
  DATA:   WL_DATA(10),
               WL_HORA(8),
               WL_LINHA(60),
               WL_TEXT TYPE SDYDO_TEXT_ELEMENT.

  WL_TEXT = 'Comparativo Fiscal Compra X Exportação'.

  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = WL_TEXT
      SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>EXTRA_LARGE
      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.



  IF P_BUKRS   IS NOT INITIAL.
    IF P_BUKRS-HIGH IS INITIAL.
      CONCATENATE 'Empresa  :' P_BUKRS-LOW
       INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Empresa  :' P_BUKRS-LOW 'à' P_BUKRS-HIGH
      INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF P_FILIAL   IS NOT INITIAL.
    IF P_FILIAL-HIGH IS INITIAL.
      CONCATENATE 'Filial  :' P_FILIAL-LOW
       INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Filial  :' P_FILIAL-LOW 'à' P_FILIAL-HIGH
      INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*          SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF P_NOMEA   IS NOT INITIAL.
    IF P_NOMEA-HIGH IS INITIAL.
      CONCATENATE 'Nomeação  :' P_NOMEA-LOW
       INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Nomeação  :' P_NOMEA-LOW 'à' P_NOMEA-HIGH
      INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*          SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF P_PERIO IS NOT INITIAL.
    CONCATENATE P_PERIO-LOW+6(2) P_PERIO-LOW+4(2) P_PERIO-LOW+0(4) INTO  WL_DATA SEPARATED BY '.'.
    IF P_PERIO-HIGH IS INITIAL.
      CONCATENATE 'Periodo  :' WL_DATA
      INTO WL_LINHA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Periodo :' WL_DATA  INTO WL_LINHA SEPARATED BY SPACE.
      CONCATENATE P_PERIO-HIGH+6(2) P_PERIO-HIGH+4(2) P_PERIO-HIGH+0(4) INTO  WL_DATA SEPARATED BY '.'.
      CONCATENATE WL_LINHA 'à' WL_DATA  INTO WL_LINHA SEPARATED BY SPACE.
    ENDIF.
    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

ENDFORM.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'UP'.
      REFRESH IT_SAIDA.
      CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
