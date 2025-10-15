PROCESS BEFORE OUTPUT.
  MODULE status_0101.
  MODULE criar_objetos_0101.
*
PROCESS AFTER INPUT.
  MODULE user_command_0101.

*-CS2022001149-29.03.2023-#103469-JT-inicio
  CHAIN.
    FIELD: vg_eindt_ini,
           vg_eindt_ini MODULE f_validar_data.
  ENDCHAIN.
*-CS2022001149-29.03.2023-#103469-JT-fim
