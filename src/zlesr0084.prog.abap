*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 16/06/2014                                              &*
*& Descrição: Carregamento automatico de arquivos L1, L2 e L3         &*
*& Transação: ZLES0103                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*

REPORT  ZLESR0084.

"Para Execução em backgound (jobs) """"""""""""""""""""""""""""
IF SY-BATCH EQ ABAP_TRUE.
  TRY .
    ZCL_JOB=>GET_CK_PROGRAM_EXECUCAO( EXPORTING I_NOME_PROGRAM = SY-CPROG IMPORTING E_QTD = DATA(E_QTD) ).
  CATCH ZCX_JOB.
    E_QTD = 1.
  ENDTRY.

  IF E_QTD GT 1.
    LEAVE PROGRAM.
  ENDIF.
ENDIF.

SUBMIT ZLESI0005 WITH R_UNIX  = 'X'
                 WITH R_LOCAL = SPACE
                 AND RETURN.

SUBMIT ZLESI0006 WITH R_UNIX  = 'X'
                 WITH R_LOCAL = SPACE
                 AND RETURN.

SUBMIT ZLESI0007 WITH R_UNIX  = 'X'
                 WITH R_LOCAL = SPACE
                 AND RETURN.

PERFORM CARGA_DE_TABELAS(ZLESR0095).
