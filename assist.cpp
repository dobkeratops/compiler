#include "assist.h"


extern void find_completions(Name n,std::function<void(Name n,int score)> f);

void assist_find_symbol(Node* n, Scope* sc, Name symbol){
	int max=10;
	MyVec<pair<Name,int>> completions;
	dbprintf("...completions:-");
	find_completions(symbol,
		[&](Name s,int score)->void{
			completions.push_back(std::make_pair(s,score));
		}
	);
	// todo sort them
	for (auto i=0; i<max && i<completions.size();i++){
		auto& c=completions[i];
		auto ni=sc->find_named_items_rec(c.first);
		if (ni){
		// TODO: sort by distance from current locatino;
		// allow searching forward too.
			for (auto fd=ni->fn_defs; fd;fd=fd->next_of_name){
				info(fd,"\t", str(fd->name)); fd->dump_signature();
			}
			for (auto sd=ni->structs; sd;sd=sd->next_of_name){
				info(sd,"\tstruct\t%s", str(sd->name)); 
			}
			for (auto fd=ni->fields; fd;fd=fd->next_of_name){
				info(fd,"\tfield\t%s.'t%s:", fd->owner->name_str(), str(fd->name)); fd->type()->dump_if(-1);
			}
		}
	}	


}
