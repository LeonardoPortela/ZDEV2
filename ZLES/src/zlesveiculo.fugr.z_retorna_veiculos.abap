FUNCTION z_retorna_veiculos.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(RT_PLACA) TYPE  ZLXHME_RANGE_C7_T OPTIONAL
*"     REFERENCE(RT_PROPR) TYPE  LXHME_RANGE_C10_T OPTIONAL
*"     REFERENCE(RT_CIDAD) TYPE  LXHME_RANGE_C25_T OPTIONAL
*"     REFERENCE(RT_ESTAD) TYPE  LXHME_RANGE_C3_T OPTIONAL
*"  TABLES
*"      IT_VEICULOS STRUCTURE  ZLEST0002
*"  EXCEPTIONS
*"      ERRO_VAZIO
*"----------------------------------------------------------------------

  CLEAR it_veiculos.

  SELECT *
    FROM zlest0002
    INTO CORRESPONDING FIELDS OF TABLE it_veiculos
   WHERE pc_veiculo   IN rt_placa
     AND proprietario IN rt_propr
     AND cd_cidade    IN rt_cidad
     AND cd_uf        IN rt_estad.

  IF sy-subrc NE 0.
    MESSAGE e000 RAISING erro_vazio.
  ENDIF.

ENDFUNCTION.
