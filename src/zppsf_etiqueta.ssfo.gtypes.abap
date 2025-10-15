TYPES:
  BEGIN OF TY_ETIQUETA,
    NITROGENIO     TYPE STRING,
    FOSFORO        TYPE STRING,
    AGUA           TYPE STRING,
    POTASSIO       TYPE STRING,
    UNDEFINED      TYPE STRING,
    LOTE           TYPE AUFK-AUFNR,
    PLACA          TYPE STRING,
    TEXTOSD_01     TYPE STRING,
    TEXTOSD_02     TYPE STRING,
    TEXTOSD_03     TYPE STRING,
    TEXTOSD_04     TYPE STRING,
    TEXTOSD_05     TYPE STRING,
    TEXTOSD_06     TYPE STRING,
    NATUREZA_FISICA TYPE STRING,
* ---> S4 Migração - 21/06/2023 - FC - Inicio
    "REGISTRO_MAPA  TYPE MARA-GROES,
    REGISTRO_MAPA  TYPE atwrt,
* <--- S4 Migração - 21/06/2023 - FC - Fim
    QTE_TON        TYPE STRING,
    FABRICADO_EM   TYPE SY-DATUM,
    PRAZO_VALIDADE TYPE SY-DATUM,
    CNPJ_H         TYPE string,
    IE_H           TYPE string,
    FILIAL_H       TYPE string,
    reg_map_h      TYPE string,
    regio_h        type string,
    cep_h          TYPE string,
    cp_h           TYPE string,
    material       type string.
    include type lfa1.
types:  END OF TY_ETIQUETA.



