*&---------------------------------------------------------------------*
*&  Include           MZHCMR0002I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: lv_dattr TYPE char10.

  FREE MEMORY ID 'LV_DATTR'.

  CASE okcode.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'LEAVE'.
      LEAVE PROGRAM.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BT_01'.
      CALL TRANSACTION 'ZHCMT0012'.
    WHEN 'BT_02'.
      CALL TRANSACTION 'ZHCMT0013'.
    WHEN 'BT_03'.
      CALL TRANSACTION 'ZHCMT0014'.
* Ini - RJF - CS2024000177 Ajuste em novos dados de instituição de ensino - APRENDIZ / ESTAGIÁRIO
    WHEN 'BT_04'.
      MOVE 'ZHCMT0016' TO lv_dattr.
      EXPORT lv_dattr FROM lv_dattr TO MEMORY ID 'LV_DATTR'.
      CALL TRANSACTION 'ZHCMT0016'. "Trans. Princ. ZHCMT0015
    WHEN 'BT_05'.
      MOVE 'ZHCMT0017' TO lv_dattr.
      EXPORT lv_dattr FROM lv_dattr TO MEMORY ID 'LV_DATTR'.
      CALL TRANSACTION 'ZHCMT0017'.
* Fim - RJF - CS2024000177 Ajuste em novos dados de instituição de ensino - APRENDIZ / ESTAGIÁRIO
  ENDCASE.

ENDMODULE.
