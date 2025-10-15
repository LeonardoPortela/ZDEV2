*&---------------------------------------------------------------------*
*&  Include           ZXCO1U01
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  TABLES
*"      HEADER_TABLE STRUCTURE  CAUFVDB
*"      HEADER_TABLE_OLD STRUCTURE  CAUFVDB
*"      POSITION_TABLE STRUCTURE  AFPOB
*"      POSITION_TABLE_OLD STRUCTURE  AAFPO
*"      SEQUENCE_TABLE STRUCTURE  AFFLB
*"      SEQUENCE_TABLE_OLD STRUCTURE  AAFFL
*"      OPERATION_TABLE STRUCTURE  AFVGB
*"      OPERATION_TABLE_OLD_AFVC STRUCTURE  AAFVC
*"      OPERATION_TABLE_OLD_AFVV STRUCTURE  AAFVV
*"      COMPONENT_TABLE STRUCTURE  RESBB
*"      COMPONENT_TABLE_OLD STRUCTURE  ARESB
*"      RELATION_TABLE STRUCTURE  AFABB OPTIONAL
*"      RELATION_TABLE_OLD STRUCTURE  AAFAB OPTIONAL
*"      PSTEXT_TABLE STRUCTURE  NPTXB OPTIONAL
*"      PSTEXT_TABLE_OLD STRUCTURE  ANPTX OPTIONAL
*"      MILESTONE_TABLE STRUCTURE  MLSTB OPTIONAL
*"      MILESTONE_TABLE_OLD STRUCTURE  AMLST OPTIONAL
*"      PLANNED_ORDER_TABLE STRUCTURE  PLAF
*"      STATUS_TABLE STRUCTURE  JEST
*"      STATUS_TABLE_OLD STRUCTURE  JEST
*"      OPERATION_RELATIONS STRUCTURE  PRE_DEC
*"      OPERATION_RELATIONS_OLD STRUCTURE  PRE_DEC
*"      OPERATION_TABLE_OLD_AFVU STRUCTURE  AAFVU
*"      DOCLINK_TABLE STRUCTURE  AFDLD
*"      DOCLINK_TABLE_OLD STRUCTURE  AFDLD
*"----------------------------------------------------------------------
DATA: STATUS_NEW TYPE FLAG,
      STATUS_OLD TYPE FLAG.

BREAK-POINT ID ZPPPI003.

CHECK HEADER_TABLE-WERKS EQ '0175'.

"Verificar se os status de sistema não são MREL, ENTE e ENCE
" I0045 PT  ENTE  Concluído tecnicamente
" I0046 PT  ENCE  Encerrado
" I0076 PT  MREL  Marcação para eliminação

READ TABLE STATUS_TABLE WITH KEY  OBJNR = HEADER_TABLE-OBJNR
                                  STAT  = 'I0045'   "Concluído tecnicamente
                                  INACT = ' '.      "não está inativo
CHECK SY-SUBRC NE 0.

READ TABLE STATUS_TABLE WITH KEY  OBJNR = HEADER_TABLE-OBJNR
                                  STAT  = 'I0046'   "Encerrado
                                  INACT = ' '.      "não está inativo
CHECK SY-SUBRC NE 0.

READ TABLE STATUS_TABLE WITH KEY  OBJNR = HEADER_TABLE-OBJNR
                                  STAT  = 'I0076'   "Marcação para eliminação
                                  INACT = ' '.      "não está inativo
CHECK SY-SUBRC NE 0.

" Tratamento dos pontos de integração com os sistemas Pró-automação
" e OPUS
" Ponto 4.4.3 – Sequenciamento da Produção
" Ponto 4.4.7 – Envio da Ordem de Produção (para o pró-automação)

" Tratamento do ponto 4.4.3 - Sequenciamento.
" Neste caso será enviada para o sistema OPUS a informação que uma
" ordem de processo foi sequenciada

READ TABLE STATUS_TABLE WITH KEY  OBJNR = HEADER_TABLE-OBJNR
                                  STAT  = 'E0002'   "Autoriação de Entrada
                                  INACT = ' '.      "não está inativo
IF SY-SUBRC EQ 0.
  STATUS_NEW = 'X'.
ENDIF.

READ TABLE STATUS_TABLE_OLD WITH KEY  OBJNR = HEADER_TABLE-OBJNR
                                      STAT  = 'E0002'   "Autoriação de Entrada
                                      INACT = ' '.      "não está inativo
IF SY-SUBRC EQ 0.
  STATUS_OLD = 'X'.
ENDIF.

IF STATUS_NEW IS NOT INITIAL AND STATUS_OLD IS INITIAL.
  " o status E0002 foi ativado nesta execução
  " acrescentar a seguir o código para o envio da informação para o Opus
  CLEAR: STATUS_NEW, STATUS_OLD.
ENDIF.

" Tratamento do ponto 4.4.7 – Envio da Ordem de Produção.
" Neste caso será enviada para o sistema pró-automação a informação que uma
" ordem de processo foi liberda e "pesada" e que o caminhão está pronto para carregar

READ TABLE STATUS_TABLE WITH KEY  OBJNR = HEADER_TABLE-OBJNR
                                  STAT  = 'E0001'   "1ª Pesagem
                                  INACT = ' '.      "não está inativo
IF SY-SUBRC EQ 0.
  STATUS_NEW = 'X'.
ENDIF.

READ TABLE STATUS_TABLE_OLD WITH KEY  OBJNR = HEADER_TABLE-OBJNR
                                      STAT  = 'E0001'   "1ª Pesagem
                                      INACT = ' '.      "não está inativo
IF SY-SUBRC EQ 0.
  STATUS_OLD = 'X'.
ENDIF.

IF STATUS_NEW IS NOT INITIAL AND STATUS_OLD IS INITIAL.
  CLEAR: STATUS_NEW, STATUS_OLD.
  " o status E0001 foi ativado nesta execução
  " acrescentar a seguir o código para o envio da informação para o pró-automação

ENDIF.

" Conclusão da produção
READ TABLE STATUS_TABLE WITH KEY  OBJNR = HEADER_TABLE-OBJNR
                                  STAT  = 'E0006'   "Conclusão da produção
                                  INACT = ' '.      "não está inativo
IF SY-SUBRC EQ 0.
  STATUS_NEW = 'X'.
ENDIF.

READ TABLE STATUS_TABLE_OLD WITH KEY  OBJNR = HEADER_TABLE-OBJNR
                                      STAT  = 'E0006'   "Conclusão da produção
                                      INACT = ' '.      "não está inativo
IF SY-SUBRC EQ 0.
  STATUS_OLD = 'X'.
ENDIF.

IF STATUS_NEW IS NOT INITIAL AND STATUS_OLD IS INITIAL.
*DATA LOTE TYPE CHARG_D.

  "//set batch ( teste )
*POSITION_TABLE[ AUFNR = HEADER_TABLE-AUFNR ]-CHARG = '1000140'.

  CLEAR: STATUS_NEW, STATUS_OLD.
ENDIF.
