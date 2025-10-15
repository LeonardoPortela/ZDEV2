@AbapCatalog.sqlViewName: 'ZHCM_001'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@Analytics.query: false
@EndUserText.label: 'ZCDS_BN0001'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #CONSUMPTION
@OData.publish : true


define view ZCDS_BN0001 
    as select distinct from pa0001 as pa0001
    
    // JOINS COM TABELAS 
    left outer join j_1bbranch as J_1BBRANCH
    on J_1BBRANCH.bukrs = pa0001.bukrs 
              
    left outer join t500p as t500p
    on t500p.bukrs = pa0001.bukrs
    and t500p.persa = pa0001.werks
    
    left outer join cskt as cskt
    on cskt.kostl = pa0001.kostl
    
    left outer join hrp1000 as hrp1000
    on hrp1000.objid = pa0001.orgeh
    
    left outer join hrp1000 as hrp1000_2
    on hrp1000_2.objid = pa0001.stell
    
    left outer join pa0002 as pa0002
    on pa0002.pernr = pa0001.pernr
    and pa0002.endda = pa0001.endda   
    
    left outer join pa0465 as pa0465
    on pa0465.pernr = pa0001.pernr 
    and pa0465.begda = pa0001.begda 
    and pa0465.endda = pa0001.endda       
    
    left outer join pa0167 as PA0167
    on PA0167.pernr = pa0001.pernr  
    and PA0167.begda = pa0001.begda 
    and PA0167.endda = pa0001.endda      
    
    left outer join t5ubi as t5ubi
    on  t5ubi.barea = PA0167.barea
    and t5ubi.bplan = PA0167.bplan
    and t5ubi.bcost = PA0167.pltyp
    
    left outer join pa0015 as pa0015
    on pa0015.pernr = pa0001.pernr   
    and pa0015.begda = pa0001.begda 
    and pa0015.endda = pa0001.endda 
    
    left outer join pa0000 as pa0000
    on pa0000.pernr = pa0001.pernr 
    and pa0000.begda = pa0001.begda 
    and pa0000.endda = pa0001.endda   
    
    left outer join t529u as T529U
    on T529U.statv = pa0000.stat2 
    and T529U.sprsl = 'P'  
        
{


        @UI.lineItem: [{  position: 140, label: 'Matrícula' }]
        @UI.selectionField: [{ position: 10 }]
    key pa0001.pernr as Pernr,
    key pa0001.subty as Subty,
    key pa0001.objps as Objps,
    key pa0001.sprps as Sprps,
        @UI.lineItem: [{  position: 180, label: 'Data Admissão ' }]
//    key pa0001.begda as Begda,
//     pa0001.begda,
        cast(
         Concat(
           Concat(
             Concat(substring(pa0001.begda, 7, 2), '/'),
             Concat(substring(pa0001.begda, 5, 2), '/')
          ),
        Substring(pa0001.begda, 1, 4)
          )
        as char10 preserving type) as Begda,
            
        @UI.lineItem: [{  position: 170, label: 'Data Desligamento ' }]
//     pa0001.endda as Endda,
//     pa0001.endda,
        cast(
         Concat(
           Concat(
             Concat(substring(pa0001.endda, 7, 2), '/'),
             Concat(substring(pa0001.endda, 5, 2), '/')
          ),
        Substring(pa0001.endda, 1, 4)
          )
        as char10 preserving type) as Endda,
            
     pa0001.seqnr as Seqnr,
        pa0001.aedtm as Aedtm,
        pa0001.uname as Uname,
        pa0001.histo as Histo,
        pa0001.itxex as Itxex,
        pa0001.refex as Refex,
        pa0001.ordex as Ordex,
        pa0001.itbld as Itbld,
        pa0001.preas as Preas,
        pa0001.flag1 as Flag1,
        pa0001.flag2 as Flag2,
        pa0001.flag3 as Flag3,
        pa0001.flag4 as Flag4,
        pa0001.rese1 as Rese1,
        pa0001.rese2 as Rese2,
        pa0001.grpvl as Grpvl,
        @UI.lineItem: [{  position: 10, label: 'Empresa' }]
        pa0001.bukrs as Bukrs,
        @UI.lineItem: [{  position: 40, label: 'Filial' }]
        pa0001.werks as Werks,
        pa0001.persg as Persg,
        pa0001.persk as Persk,
        pa0001.vdsk1 as Vdsk1,
        pa0001.gsber as Gsber,
        pa0001.btrtl as Btrtl,
        pa0001.juper as Juper,
        pa0001.abkrs as Abkrs,
        pa0001.ansvh as Ansvh,
        @UI.lineItem: [{  position: 60, label: 'Centro de Custo' }]
        pa0001.kostl as Kostl,
        @UI.lineItem: [{  position: 80, label: 'Unidade Organizacional' }]
        @UI.selectionField: [{ position: 60 }]
        pa0001.orgeh as Orgeh,
        pa0001.plans as Plans,
        @UI.lineItem: [{  position: 110, label: 'Cargo' }]
        pa0001.stell as Stell,
        pa0001.mstbr as Mstbr,
        pa0001.sacha as Sacha,
        pa0001.sachp as Sachp,
        pa0001.sachz as Sachz,
        pa0001.sname as Sname,
        pa0001.ename as Ename,
        pa0001.otype as Otype,
        pa0001.sbmod as Sbmod,
        pa0001.kokrs as Kokrs,
        pa0001.fistl as Fistl,
        pa0001.geber as Geber,
        pa0001.fkber as Fkber,
        pa0001.grant_nbr as GrantNbr,
        pa0001.sgmnt as Sgmnt,
        pa0001.budget_pd as BudgetPd,
        @UI.lineItem: [{  position: 20, label: 'Empresa' }]
        @UI.selectionField: [{ position: 20 }]
//        'xxxx' as Empresa, 
        j_1bbranch.name as Empresa,
        @UI.lineItem: [{  position: 30, label: 'CNPJ' }]
//        'xxxx' as CNPJ,
        j_1bbranch.stcd1 as CNPJ,
        @UI.lineItem: [{  position: 50, label: 'Desc. Filial' }]
//        'xxx' as DescFilial,
        t500p.name1 as DescFilial,
        @UI.lineItem: [{  position: 70, label: 'Desc. Centro de Custo' }]
//        'xxxx' as DescCC,
        cskt.ltext as DescCC,
        @UI.lineItem: [{  position: 90, label: 'Desc. Unidade Organizacional' }]
//        'xxxx' as DescUO,
        hrp1000.stext as DescUO,
        @UI.lineItem: [{  position: 100, label: 'Diretoria' }]
        'Diretoria' as Diretoria,
        
//        fldate,
//              seatsocc_f,
//              @ObjectModel.readOnly: true
//              @ObjectModel.virtualElement: true 
//              @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_HCM_UTIL' //ZCL_HCM_UTIL
//              cast( '' as abap.char(255)) as text,
//            
////        }where i_pernr = pa0001.pernr
        
        @UI.lineItem: [{  position: 110, label: 'Desc. Diretoria' }]
        'Desc Diretoria' as DescDiretoria,
        @UI.lineItem: [{  position: 130, label: 'Desc. Cargo' }]
//        'xxxx' as DescCargo,
        hrp1000_2.stext as DescCargo,
        @UI.lineItem: [{  position: 140, label: 'Nome' }]
//        'xxxx' as Nome,
        pa0002.cname as Nome,
        @UI.lineItem: [{  position: 150, label: 'CPF' }]
//        'xxxx' as CPF,
        pa0465.cpf_nr as CPF,
        @UI.lineItem: [{  position: 160, label: 'Situação' }]
        T529U.text1 as Situacao,
        @UI.lineItem: [{  position: 190, label: 'Sexo' }]
//        'Sexo' as Sexo,
        case pa0002.gesch
            when '1' then 'Masculino'
            when '2' then 'Feminino'
            else ' '
        end as Sexo,
        
        @UI.lineItem: [{  position: 200, label: 'Tipo Plano ' }]
        pa0167.subty as TipoPlano,
        @UI.lineItem: [{  position: 210, label: 'Tipo Benefício' }]
        pa0167.bplan as TipoBenef,
        @UI.lineItem: [{  position: 220, label: 'Cobertura Dep.' }]
        pa0167.depcv as Cobertura,
        @UI.lineItem: [{  position: 230, label: 'Custo Empregado' }]
//        t5ubi.accfa as CEmpregado,
        t5ubi.eecst as CEmpregado,
        @UI.lineItem: [{  position: 240, label: 'Custo Empregador' }]
        t5ubi.ercst as CEmpregador,
        @UI.lineItem: [{  position: 250, label: 'Custo Prestador ' }]
        t5ubi.accst as CustoPrestador,
        @UI.lineItem: [{  position: 260, label: 'Coparticipação ' }]
        pa0015.betrg as Coparticip,
        @UI.selectionField: [{ position: 30 }]   
        pa0001.werks as AreaRH,
        @UI.selectionField: [{ position: 40 }]   
        pa0000.stat2 as StatusOcup,
        @UI.selectionField: [{ position: 50 }]   
        pa0001.kostl as CentroCusto, 
        @UI.selectionField: [{ position: 70 }]   
        pa0001.stell as Cargo
}
//    where pa0001.plans <> '99999999'
//    group by pa0001.pernr
//    union pa0001.pernr
