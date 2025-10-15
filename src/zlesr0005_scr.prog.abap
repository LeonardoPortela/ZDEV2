*&--------------------------------------------------------------------&*
*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Nilton Marcelo Segantin                                 &*
*& Data.....: 10/10/2025                                              &*
*& Descrição: Serviço de Frete de Terceiros                           &*
*& Transação: ZLES0224 (Prest. Serv. Frete - Faturar)                 &*
*---------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2T37 |10/10/2025 |Desenvolvimento Inicial.       &*
*&--------------------------------------------------------------------&*
**********************************************************************
* Tela de Pesquisa (0101)
**********************************************************************
  SELECTION-SCREEN BEGIN OF SCREEN 101 AS SUBSCREEN.
* Tela de seleção
    SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

      SELECTION-SCREEN BEGIN OF LINE.
        SELECTION-SCREEN COMMENT 01(20) TEXT-002 FOR FIELD p_bukrs. "Empresa
        SELECTION-SCREEN POSITION 25.
        PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY.
        SELECTION-SCREEN PUSHBUTTON 77(19) btn_pesq USER-COMMAND pesq.

      SELECTION-SCREEN END OF LINE.

      SELECTION-SCREEN BEGIN OF LINE.
        SELECTION-SCREEN COMMENT 01(21) TEXT-003 FOR FIELD s_branch. "Filial Transportadora
        SELECTION-SCREEN POSITION 22.
        SELECT-OPTIONS s_branch FOR zlest0257-branch NO INTERVALS.
        SELECTION-SCREEN COMMENT 60(14) TEXT-004 FOR FIELD s_client. "Cliente
        SELECTION-SCREEN POSITION 75.
        SELECT-OPTIONS s_client FOR zlest0257-id_cliente NO INTERVALS MODIF ID obl.

      SELECTION-SCREEN END OF LINE.

      SELECTION-SCREEN BEGIN OF LINE.
        SELECTION-SCREEN COMMENT 01(21) TEXT-005 FOR FIELD s_id_neg. "ID Negociação
        SELECTION-SCREEN POSITION 22.
        SELECT-OPTIONS s_id_neg FOR zlest0257-id_neg NO INTERVALS.
        SELECTION-SCREEN COMMENT 60(13) TEXT-006 FOR FIELD s_dtmov. "Período
        SELECTION-SCREEN POSITION 75.
        SELECT-OPTIONS s_dtmov FOR zlest0257-dt_movimento NO INTERVALS MODIF ID obl. "Período

      SELECTION-SCREEN END OF LINE.

      SELECTION-SCREEN BEGIN OF LINE.
        SELECTION-SCREEN COMMENT 01(21) TEXT-012 FOR FIELD s_ordvnd. "OV
        SELECTION-SCREEN POSITION 22.
        SELECT-OPTIONS s_ordvnd FOR zlest0257-vbeln NO INTERVALS.

      SELECTION-SCREEN END OF LINE.

      SELECTION-SCREEN SKIP.
* Tipo do status.
      SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-010.
        SELECTION-SCREEN BEGIN OF LINE.
          PARAMETERS rb_pend TYPE c RADIOBUTTON GROUP grp.
          SELECTION-SCREEN COMMENT 02(10) TEXT-007 FOR FIELD rb_pend. "Pendente
          SELECTION-SCREEN POSITION 38.
          PARAMETERS rb_fatur TYPE c RADIOBUTTON GROUP grp.
          SELECTION-SCREEN COMMENT 60(10) TEXT-008 FOR FIELD rb_fatur. "Faturada
          PARAMETERS rb_todas TYPE c RADIOBUTTON GROUP grp.
          SELECTION-SCREEN COMMENT 73(10) TEXT-009 FOR FIELD rb_todas. "Todas
        SELECTION-SCREEN END OF LINE.

      SELECTION-SCREEN END OF BLOCK b2.

    SELECTION-SCREEN END OF BLOCK b1.

  SELECTION-SCREEN END OF SCREEN 101.
