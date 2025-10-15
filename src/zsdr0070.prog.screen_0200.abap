PROCESS BEFORE OUTPUT.
  MODULE pbo_0200.
  MODULE trata_fields.

PROCESS AFTER INPUT.
  MODULE pai_0200.
  MODULE user_command_0200_exit AT EXIT-COMMAND.

  CHAIN.
    FIELD wa_novo-empresa.
    FIELD wa_novo-cliente.
    FIELD wa_novo-vendedor.
    FIELD wa_novo-corretor.
    FIELD wa_novo-tipo_padrao.
    FIELD wa_novo-acts.
    MODULE atualiza_dados ON CHAIN-REQUEST.
  ENDCHAIN.


  FIELD wa_novo-visao MODULE trata_fields_input ON INPUT.
  FIELD wa_novo-intercompany MODULE trata_fields_input ON INPUT.
  FIELD wa_novo-id_contrato_referencia MODULE trata_fields_input ON
INPUT.
  FIELD wa_novo-cliente MODULE trata_fields_input ON INPUT.
  FIELD wa_novo-acts    MODULE trata_fields_input ON INPUT.

PROCESS ON VALUE-REQUEST.

  FIELD wa_novo-visao MODULE create_dropdown_box.
  FIELD wa_novo-id_contrato_referencia MODULE search_id_cont_ref.
*PROCESS ON VALUE-REQUEST.
*  FIELD WA_NOVO-PORTO MODULE SEARCH_PORTO.
