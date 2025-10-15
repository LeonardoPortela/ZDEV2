PROCESS BEFORE OUTPUT.

  MODULE tratar_field.
  MODULE status_0100.
*
PROCESS AFTER INPUT.

  CHAIN.
    FIELD  wa_zfiwrt0021-tipo       MODULE busca_forma_tipo
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-bukrs      MODULE busca_forma_bukrs
     ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-branch     MODULE busca_forma_branch
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-kunnr      MODULE busca_forma_kunnr
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-matnr      MODULE busca_forma_matnr
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-operacao   MODULE busca_forma_operacao
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-tarifa01   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-montante01   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-tarifa02   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-montante02   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-tarifa03   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-montante03   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-tarifa04   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-montante04   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-tarifa05   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-montante05   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-tarifa06   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-montante06   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-tarifa07   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-montante07   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-tarifa08   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-montante08   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-tarifa09   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-montante09   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-tarifa10   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-montante10   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-tarifa11   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-montante11   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-tarifa12   MODULE busca_total
    ON CHAIN-INPUT.
    FIELD  wa_zfiwrt0021-montante12   MODULE busca_total
    ON CHAIN-INPUT.
  ENDCHAIN.

  MODULE user_command_0100.


PROCESS ON VALUE-REQUEST.

  FIELD wa_zfiwrt0021-tipo     MODULE busca_tipo_contrato.
  FIELD wa_zfiwrt0021-contrato MODULE busca_contrato.
  FIELD wa_zfiwrt0021-branch   MODULE busca_branch.
  FIELD wa_zfiwrt0021-operacao MODULE busca_operacao.
  FIELD wa_zfiwrt0021-banco    MODULE busca_banco.
