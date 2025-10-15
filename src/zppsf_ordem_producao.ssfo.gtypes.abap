TYPES: BEGIN OF TY_GENERIC_LIST,
         IDNRK      TYPE STPO-IDNRK, "/COMPONENT
         DESCR      TYPE MAKT-MAKTX, "/DESCRIÇÃO
         MENGE      TYPE P,     "/QUANTIDADE
         TOTAL      TYPE P,
         MEINS      TYPE STRING,     "/UNID. MEDIDA
         NITROGENIO TYPE STRING,"/NITROGENIO
         FOSFORO    TYPE STRING,     "/FOSFORO
         POTASSIO   TYPE STRING,   "/POTASSIO
         CALCIO     TYPE STRING,    "/CALCIO
         MAGNESIO   TYPE STRING,  "/MAGNESIO
         ENXOFRE    TYPE STRING,  "/ENXOFRE
         BORO       TYPE STRING,     "/BORO
         ZINCO      TYPE STRING,      "/ZINCO
         MOLIBDENIO TYPE STRING,"/MOLIBDENIO
         COBRE      TYPE STRING,     "/COBRE
         MANGANES   TYPE STRING,    "/MANGANES
         SILICIO    TYPE STRING,  "/SILÍCIO
         AGUA       TYPE STRING,        "/AGUA
       END OF TY_GENERIC_LIST,

       BEGIN OF TY_ORDER,
         ORDEM_PRODUCAO     TYPE AUFK-AUFNR,
         ORDEM_CARREGAMENTO TYPE AUFK-ORDEMCARREG,
         EMPRESA            TYPE T001-BUTXT,
         FILIAL             TYPE J_1BBRANCH-NAME,
         PLACA              TYPE AUFK-PLACA,
         PEDIDO             TYPE VBKD-BSTKD,
         REGISTRO_MAPA      TYPE MARA-GROES,
         DATA               TYPE SY-DATUM,
         CLIENTE            TYPE VBAK-KUNNR,
         NOME               TYPE KNA1-NAME1,
         LOTE               TYPE AUFK-AUFNR,
         MATERIAL           TYPE AFPO-MATNR,
         TEXTO              TYPE AUFK-KTEXT,
         QUANTIDADE         TYPE p,
         USERNAME           TYPE V_USERNAME-NAME_TEXT,
       END OF TY_ORDER.
