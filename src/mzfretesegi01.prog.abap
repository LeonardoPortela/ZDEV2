*&---------------------------------------------------------------------*
*&  Include           MZFRETESEGI01
*&---------------------------------------------------------------------*

*&SPWIZARD: INPUT MODUL FOR TC 'TAB_PESQUISA'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tab_pesquisa_mark INPUT.
*  DATA: g_tab_pesquisa_wa2 LIKE LINE OF it_valor_seg.
*  IF tab_pesquisa-line_sel_mode = 1
*  AND it_valor_seg-mark = 'X'.
*    LOOP AT it_valor_seg INTO g_tab_pesquisa_wa2
*      WHERE mark = 'X'.
*      g_tab_pesquisa_wa2-mark = ''.
*      MODIFY it_valor_seg
*        FROM g_tab_pesquisa_wa2
*        TRANSPORTING mark.
*    ENDLOOP.
*  ENDIF.
  MODIFY it_valor_seg
    INDEX tab_pesquisa-current_line
    TRANSPORTING mark.
ENDMODULE.                    "TAB_PESQUISA_MARK INPUT
