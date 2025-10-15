PROCESS BEFORE OUTPUT.
  MODULE status_0110.
*
PROCESS AFTER INPUT.
  MODULE user_command_0110.

  CHAIN.
    FIELD l_cod_motorista.
    MODULE clear_screen.
  ENDCHAIN.

*-CS2024000522-12.09.2024-JT-#152417-inicio
  CHAIN.
    FIELD: l_nro_ordem_car,
           l_safra_ordem_car
    MODULE f_atualiza_campos_modal.
  ENDCHAIN.
*-CS2024000522-12.09.2024-JT-#152417-fim

  FIELD l_cod_motorista MODULE f_cod_motorista.
