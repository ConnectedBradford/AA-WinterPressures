
//Rcpp file for the same-named R file

#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame gen_emergency_patients(DataFrame emergency_gen_table, DataFrame emergency_spells, int searchTimeWindow, int searchDateWindow) {

 //searchDateWindow in days
 //searchTimeWindow in seconds
 
 //returns a dataframe of spell_ids that can be put back into the emergency_gen_table in R (not a vector as it behaves oddly)
 
 // remember everything is zero-indexed here!
 
 IntegerVector gen_spell_ids = emergency_gen_table["spell_id"];
 IntegerVector gen_y2kdate = emergency_gen_table["y2kdate"]; // days since index date (not seconds)
 LogicalVector gen_bizday = emergency_gen_table["bizday"];
 IntegerVector gen_time = emergency_gen_table["idx_time"]; // behaves nicely and casts to integer
 
 IntegerVector sp_spell_ids = emergency_spells["_SpellID"];
 LogicalVector sp_bizday = emergency_spells["_bizday"];
 IntegerVector sp_y2kdate = emergency_spells["_2KMD_Date"];
 IntegerVector sp_time = emergency_spells["_Start_Time"];
 
 int n_spells = emergency_spells.nrow()-1;
 int n_gens = emergency_gen_table.nrow()-1;
 
 int rnd_spells [n_spells];
 
 for (int gen_i=0;gen_i<=n_gens;gen_i++){
   //iterate over each gen slot
   
   //Fisher-Yates inside-out to produce random ordering of spells
   for(int i=0;i<=n_spells;i++) {
     int rnd=unif_rand() * i;
     rnd_spells[i] = rnd_spells[rnd];
     rnd_spells[rnd] = i;
   }
   
   int startTsearch = gen_time[gen_i] - searchTimeWindow;
   int endTsearch = gen_time[gen_i] + searchTimeWindow;
   int startDsearch = gen_y2kdate[gen_i] - searchDateWindow;
   int endDsearch = gen_y2kdate[gen_i] + searchDateWindow;
   
   
   for (int sp_i=0;sp_i<=n_spells;sp_i++){
     //find the first spell that is a match (order by rnd_spells)
     //rnd_spells[sp_i] is the index of the spell we're looking at
     //gen_i is the index of the one we're generating
     
     int sp_rnd_i=rnd_spells[sp_i];
     
     if (sp_bizday[sp_rnd_i]==gen_bizday[gen_i]) { //easiest check here
       if ((sp_y2kdate[sp_rnd_i]>startDsearch && sp_y2kdate[sp_rnd_i]<endDsearch) || ((sp_y2kdate[sp_rnd_i]+365)>startDsearch && (sp_y2kdate[sp_rnd_i]+365)<endDsearch) || ((sp_y2kdate[sp_rnd_i]-365)>startDsearch && (sp_y2kdate[sp_rnd_i]-365)<endDsearch)) {
         if ((sp_time[sp_rnd_i]>startTsearch && sp_time[sp_rnd_i]<endTsearch) || ((sp_time[sp_rnd_i]+86400)>startTsearch && (sp_time[sp_rnd_i]+86400)<endTsearch) || ((sp_time[sp_rnd_i]-86400)>startTsearch && (sp_time[sp_rnd_i]-86400)<endTsearch)) {
           // we have a match - copy it and go on to the next generated episode
           gen_spell_ids[gen_i]=sp_spell_ids[sp_rnd_i];
           break;
           
           
         }
         
       }
       
       
     }
     
     
     
   }
   
 }
 
 // random debug rubbish here:
 // gen_spell_ids[1]=searchDateWindow;
 // gen_spell_ids[0]=searchTimeWindow;
 // gen_spell_ids[3]=gen_y2kdate[1];
 // gen_spell_ids[4]=gen_time[2];
 // gen_spell_ids[5]=sp_y2kdate[1];
 // gen_spell_ids[6]=sp_spell_ids[1];
 // gen_spell_ids[7]=rnd_spells[24];
 // gen_spell_ids[8]=rnd_spells[25];
 // gen_spell_ids[9]=n_gens;
 return DataFrame::create(_["spell_id"]=gen_spell_ids);

}
            
 