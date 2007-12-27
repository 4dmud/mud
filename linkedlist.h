//
// C++ Implementation: linkedlist
//
// Description: 
//
//
// Author: Jamie Nelson <mordecai@xtra.co.nz>, (C) 2006
//
// Copyright: See COPYING file that comes with this distribution
//
//
#include <iterator>


template <class T>
class List
{
	struct Node
	{
		Node(const T& x,Node* y = 0):m_data(x),m_next(y){}
		T m_data;
		Node* m_next;
	};

	Node* m_head;
public:

	class iterator 
		: public std::iterator<std::forward_iterator_tag, T>
	{
		Node* m_rep;
	public:
		friend class const_iterator;
		friend class List;

		inline iterator(Node* x=0):m_rep(x){}
		inline iterator(const iterator& x):m_rep(x.m_rep) {}
		inline iterator& operator=(const iterator& x)
		{ 
			m_rep=x.m_rep; return *this; 
		}
		inline iterator& operator++()
		{ 
			m_rep = m_rep->m_next; return *this; 
		}
		inline iterator operator++(int)
		{ 
			iterator tmp(*this); m_rep = m_rep->m_next; return tmp; 
		}
		inline T& operator*() const { return m_rep->m_data; }
		inline T* operator->() const { return m_rep; }
		inline bool operator==(const iterator& x) const
		{
			return m_rep == x.m_rep; 
		}	
		inline bool operator!=(const iterator& x) const
		{
			return m_rep != x.m_rep; 
		}	

	};

	class const_iterator 
		: public std::iterator<std::forward_iterator_tag, const T> 
	{
		const Node* m_rep;
	public:
		friend class iterator;
		friend class List;

		inline const_iterator(const Node* x=0):m_rep(x){}
		inline const_iterator(const const_iterator& x):m_rep(x.m_rep) {}
		inline const_iterator(const iterator& x):m_rep(x.m_rep){}
		inline const_iterator& operator=(const const_iterator& x)
		{ 
			m_rep=x.m_rep; return *this; 
		}
		inline const_iterator& operator=(const iterator& x)
		{ 
			m_rep=x.m_rep; return *this; 
		}		
		inline const_iterator& operator++()
		{ 
			m_rep = m_rep->m_next; return *this; 
		}
		inline const_iterator operator++(int)
		{ 
			const_iterator tmp(*this); m_rep = m_rep->m_next; return tmp; 
		}
		inline T& operator*() const { return m_rep->m_data; }
		inline T* operator->() const { return m_rep; }
		inline bool operator==(const const_iterator& x) const
		{
			return m_rep == x.m_rep; 
		}
		inline bool operator!=(const const_iterator& x) const
		{
			return m_rep != x.m_rep; 
		}



	};


	List() : m_head(0) {}

	List(const List& L) : m_head(0)
	{
		for ( const_iterator i = L.begin(); i!=L.end(); ++i )
			push_front(*i);
		reverse();
	}

	void reverse()
	{
		Node* p = 0; Node* i = m_head; Node* n;
		while (i)
		{
			n = i->m_next;
			i->m_next = p;
			p = i; i = n;
		}
		m_head = p;
	}

	void swap(List& x)
	{
		Node* tmp = m_head; m_head = x.m_head; x.m_head = tmp;
	}

	List& operator=(const List& x)
	{
		List tmp(x);
		swap(tmp);
		return *this;
	}

	~List() { clear(); }
	void clear() { while (!empty()) pop_front(); }



	inline void push_front(const T&x)
	{
		Node* tmp = new Node(x);
		tmp->m_next = m_head;
		m_head = tmp;
	}
	inline void pop_front()
	{
		if (m_head)
		{
			Node* newhead = m_head->m_next;
			delete m_head;
			m_head = newhead;
		}
	}
	inline bool empty() { return m_head; }

	inline T& front() { return *begin(); }
	inline const T& front() const { return *begin(); }

	inline iterator begin() { return iterator(m_head); }
	inline iterator end() { return iterator(); }
	inline const_iterator begin() const { return m_head; }
	inline const_iterator end() const { return const_iterator(); }

	void erase_after (iterator& x)
	{
		Node* tmp = x.m_rep->m_next;
		if (x.m_rep->m_next) 
			x.m_rep->m_next = x.m_rep->m_next->m_next;
		delete tmp;
	}

	void insert_after (iterator& x, const T& y)
	{
		Node* tmp = new Node(y,x.m_rep->m_next);
		x.m_rep->m_next = tmp;
	}
};
