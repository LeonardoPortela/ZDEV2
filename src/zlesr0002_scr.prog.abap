*&--------------------------------------------------------------------&*
*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Nilton Marcelo Segantin                                 &*
*& Data.....: 22/09/2025                                              &*
*& Descrição: Serviço de Frete de Terceiros                           &*
*& Transação: ZLES0223 (Prest. Serv. Frete Terceiros)                 &*
*---------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2T37 |22/09/2025 |Desenvolvimento Inicial.       &*
*&--------------------------------------------------------------------&*
**********************************************************************
* Tela de Pesquisa (0101)
**********************************************************************
  SELECTION-SCREEN BEGIN OF SCREEN 101 AS SUBSCREEN.
* Tela de seleção
    SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

      SELECTION-SCREEN BEGIN OF LINE.
        SELECTION-SCREEN COMMENT 01(20) TEXT-002 FOR FIELD p_bukrs. "Empresa Prest. Serv.
        SELECTION-SCREEN POSITION 25.
        PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY .
        SELECTION-SCREEN PUSHBUTTON 77(19) btn_pesq USER-COMMAND pesq.

      SELECTION-SCREEN END OF LINE.

      SELECTION-SCREEN BEGIN OF LINE.
        SELECTION-SCREEN COMMENT 01(21) TEXT-006 FOR FIELD s_branch. "Filial Transportadora
        SELECTION-SCREEN POSITION 22.
        SELECT-OPTIONS s_branch FOR zlest0255-branch NO INTERVALS.
        SELECTION-SCREEN COMMENT 60(14) TEXT-003 FOR FIELD s_ano. "Ano Negociação
        SELECTION-SCREEN POSITION 75.
        SELECT-OPTIONS s_ano FOR zlest0255-ano NO INTERVALS MODIF ID obl.

      SELECTION-SCREEN END OF LINE.

      SELECTION-SCREEN BEGIN OF LINE.
        SELECTION-SCREEN COMMENT 01(21) TEXT-008 FOR FIELD s_client. "Cliente
        SELECTION-SCREEN POSITION 22.
        SELECT-OPTIONS s_client FOR zlest0255-id_cliente NO INTERVALS MODIF ID obl.
        SELECTION-SCREEN COMMENT 60(13) TEXT-007 FOR FIELD s_id_neg. "ID Negociação
        SELECTION-SCREEN POSITION 75.
        SELECT-OPTIONS s_id_neg FOR zlest0255-id_neg NO INTERVALS.

      SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN END OF BLOCK b1.

  SELECTION-SCREEN END OF SCREEN 101.
