
PROCESS BEFORE OUTPUT.
* MODULE STATUS_0110.
*
PROCESS AFTER INPUT.
* MODULE USER_COMMAND_0110.

  CHAIN.
    FIELD zaa_controle_doc-endereco.
    FIELD zaa_controle_doc-bairro.
    FIELD zaa_controle_doc-municipio.
    FIELD zaa_controle_doc-pais.
    FIELD zaa_controle_doc-estado.
    FIELD zaa_controle_doc-matricula.
    FIELD zaa_controle_doc-area.
    FIELD zaa_controle_doc-cartorio.
    FIELD zaa_controle_doc-comarca.
    FIELD zaa_controle_doc-ccir.
    FIELD zaa_controle_doc-feins.
    FIELD zaa_controle_doc-estado_com.
    FIELD zaa_controle_doc-nro_casco.
    FIELD zaa_controle_doc-reb.
    MODULE controle ON CHAIN-REQUEST.
  ENDCHAIN.
