package menthor.util;

import gnu.trove.map.hash.TObjectDoubleHashMap;
import gnu.trove.procedure.TObjectDoubleProcedure;

import java.util.Iterator;
import java.util.List;

public class TMerger {
	static <T> TObjectDoubleHashMap<T> merge(List<TObjectDoubleHashMap<T>> hashmaps) {
		final TObjectDoubleHashMap<T> result = new TObjectDoubleHashMap<T>(hashmaps.get(0).size());  
		
		for (TObjectDoubleHashMap<T> hashmap : hashmaps) {
			hashmap.forEachEntry(new TObjectDoubleProcedure<T>() {
				@Override
				public boolean execute(T key, double value) {
					result.put(key, result.get(key) + value);
					return true;
				}
			});
		}
		
		return result;
	}	
}
