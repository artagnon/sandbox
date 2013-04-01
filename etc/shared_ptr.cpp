template <class T>

class SharedPtr{
public:
	SharedPtr(){
		nRefCount=new int;
		(*nRefCount)=1;
		data=0;
	}
	SharedPtr(T* SpArgument){
		nRefCount=new int;
		this->data=SpArgument;
		(*nRefCount)=1;
	}
	SharedPtr(const SharedPtr &SpArgument){
		this->data = SpArgument.data;
		this->nRefCount = SpArgument.nRefCount;
		(*nRefCount)++;
	}
	SharedPtr& operator=(const SharedPtr &SpArgument){
		// Emulate dtor call, destroy this
		(*nRefCount)--;
		if((*nRefCount)==0){
			delete nRefCount;
			delete data;
		}

		this->data = SpArgument.data;
		this->nRefCount = SpArgument.nRefCount;
		(*nRefCount)++;
		return *this;
	}
	T* get() const{
		return data;
	}
	T& operator*() const{
		return *data;
	}
	T* operator->() const{
		return data;
	}
	~SharedPtr(){
		(*nRefCount)--;
		if((*nRefCount)==0){
			delete nRefCount;
			delete data;
		}
	}
private:
	T* data;
	int* nRefCount;
};
