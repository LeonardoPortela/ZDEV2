*&--------------------------------------------------------------------&*
*&                        FI                                         &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Izyan Nascimento                                        &*
*& Data.....: 30/09/2015                                              &*
*& Descrição: Atualização ZIB_CONTABIL                                &*
*& Transação: ZFI                                                     &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT  ZFI0030.

TYPE-POOLS: VRM, ICON.
*=============================================================================*
*TABELAS                                                                      *
*=============================================================================*
TABLES: ZIB_CONTABIL_ERR.


*=============================================================================*
*Estrutura                                                                    *
*=============================================================================*


*=============================================================================*
*TABELA INTERNA                                                               *
*=============================================================================*
DATA IT_ZIB_CONTABIL_ERR TYPE TABLE OF ZIB_CONTABIL_ERR.
DATA: VAR_OBJ_KEY TYPE ZIB_CONTABIL_ERR-OBJ_KEY.
*=============================================================================*
*WORK AREA                                                                    *
*=============================================================================*
DATA: WA_ZIB_CONTABIL_ERR TYPE ZIB_CONTABIL_ERR.

*=============================================================================*
*Tela_Seleção                                                                 *
*=============================================================================*
SELECTION-SCREEN:  BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_OBJKEY TYPE ZIB_CONTABIL_ERR-OBJ_KEY .

SELECTION-SCREEN: END OF BLOCK B1.

*=============================================================================*
*Start-Of-Selection                                                           *
*=============================================================================*
START-OF-SELECTION.
  PERFORM:    SELECIONA_DADOS,                              "Seleção de dados
              ORGANIZA_DADOS.                               "Organiza dados

END-OF-SELECTION.
*  CALL SCREEN 0100.

*=============================================================================*
*Form F_SELECIONA_DADOS                                                       *
*=============================================================================*
FORM SELECIONA_DADOS.

  SELECT *
    FROM ZIB_CONTABIL_ERR
    INTO TABLE IT_ZIB_CONTABIL_ERR
    WHERE OBJ_KEY = P_OBJKEY.

ENDFORM.                    "SELECIONA_DADOS
FORM ORGANIZA_DADOS.

   READ TABLE IT_ZIB_CONTABIL_ERR INTO WA_ZIB_CONTABIL_ERR WITH KEY OBJ_KEY = P_OBJKEY .

    VAR_OBJ_KEY = WA_ZIB_CONTABIL_ERR-OBJ_KEY.

    IF VAR_OBJ_KEY IS INITIAL.
      MESSAGE TEXT-I01 TYPE 'I'.

    ELSE.
      DELETE FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY EQ VAR_OBJ_KEY.

    ENDIF.


    CALL FUNCTION 'Z_FI_DOCUMENT_REFRESH'
    EXPORTING
      I_OBJ_KEY = P_OBJKEY.

  ENDFORM.
