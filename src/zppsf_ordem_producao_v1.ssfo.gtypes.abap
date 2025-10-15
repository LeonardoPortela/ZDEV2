TYPES: BEGIN OF ty_generic_list,
         idnrk       TYPE stpo-idnrk,   "/COMPONENT
         descr       TYPE makt-maktx,   "/DESCRIÇÃO
         menge       TYPE string, "/QUANTIDADE
         total       TYPE p, "/CONSUMO TOTAL
         meins       TYPE string,       "/UNID. MEDIDA
         nitrogenio  TYPE string,       "/NITROGENIO
         fosforo     TYPE string,       "/FOSFORO
         potassio    TYPE string,       "/POTASSIO
         calcio      TYPE string,       "/CALCIO
         magnesio    TYPE string,       "/MAGNESIO
         enxofre     TYPE string,       "/ENXOFRE
         enxofre_so4 TYPE string,      "/ENXOFRE SO4
         boro        TYPE string,       "/BORO
         zinco       TYPE string,       "/ZINCO
         molibdenio  TYPE string,       "/MOLIBDENIO
         cobre       TYPE string,       "/COBRE
         manganes    TYPE string,       "/MANGANES
         silicio     TYPE string,       "/SILÍCIO
         agua        TYPE string,       "/AGUA
         peneira_48  TYPE string,       "/PENEIRA 4,8mm
         peneira_20  TYPE string,       "/PENEIRA 2,0mm
         peneira_10  TYPE string,       "/PENEIRA 1,0mm
       END OF ty_generic_list,

       BEGIN OF ty_order,
         ordem_producao     TYPE aufk-aufnr,
         ordem_carregamento TYPE aufk-ordemcarreg,
         empresa            TYPE t001-butxt,
         filial             TYPE j_1bbranch-name,
         placa              TYPE aufk-placa,
         pedido             TYPE vbkd-bstkd,
* ---> S4 Migração - 21/06/2023 - FC - Inicio
         "REGISTRO_MAPA  TYPE MARA-GROES,
         registro_mapa      TYPE atwrt,
* <--- S4 Migração - 21/06/2023 - FC - Fim
         data               TYPE sy-datum,
         cliente            TYPE vbak-kunnr,
         nome               TYPE kna1-name1,
         lote               TYPE aufk-aufnr,
         material           TYPE afpo-matnr,
         texto              TYPE aufk-ktext,
         quantidade         TYPE p,
         username           TYPE v_username-name_text,
       END OF ty_order.
