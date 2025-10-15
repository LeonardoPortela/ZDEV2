PROCESS BEFORE OUTPUT.
  MODULE status_0200.
*
PROCESS AFTER INPUT.
  FIELD  g_dt_lista_de   MODULE f_checar_lista_de.
  FIELD  g_dt_lista_ate  MODULE f_checar_lista_ate.
  FIELD: g_dt_lista_de,
         g_dt_lista_ate  MODULE f_checar_lista.

  MODULE user_command_0200.
