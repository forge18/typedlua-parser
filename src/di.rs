use std::any::{Any, TypeId};
use std::sync::Arc;

pub enum ServiceLifetime {
    Transient,
    Singleton,
}

struct FactoryData {
    factory: Arc<dyn Fn(&mut DiContainer) -> Arc<dyn Any + Send + Sync> + Send + Sync>,
    lifetime: ServiceLifetime,
}

pub struct DiContainer {
    factories: std::collections::HashMap<TypeId, FactoryData>,
    singletons: std::collections::HashMap<TypeId, Arc<dyn Any + Send + Sync>>,
}

impl DiContainer {
    pub fn new() -> Self {
        Self {
            factories: std::collections::HashMap::new(),
            singletons: std::collections::HashMap::new(),
        }
    }

    pub fn register<T>(
        &mut self,
        factory: impl Fn(&mut DiContainer) -> T + 'static + Send + Sync,
        lifetime: ServiceLifetime,
    ) where
        T: Send + Sync + 'static,
    {
        let type_id = TypeId::of::<T>();
        let factory: Arc<dyn Fn(&mut DiContainer) -> Arc<dyn Any + Send + Sync> + Send + Sync> =
            Arc::new(move |container| {
                let instance: T = factory(container);
                Arc::new(instance) as Arc<dyn Any + Send + Sync>
            });
        self.factories
            .insert(type_id, FactoryData { factory, lifetime });
    }

    pub fn resolve<T>(&mut self) -> Option<T>
    where
        T: Clone + Send + Sync + 'static,
    {
        let type_id = TypeId::of::<T>();

        if let Some(factory_data) = self.factories.get(&type_id) {
            match factory_data.lifetime {
                ServiceLifetime::Singleton => {
                    if let Some(cached) = self.singletons.get(&type_id) {
                        return cached.downcast_ref::<T>().cloned();
                    }

                    let factory = factory_data.factory.clone();
                    let instance = factory(self);
                    let result = instance.downcast_ref::<T>().cloned();
                    self.singletons.insert(type_id, instance);
                    result
                }
                ServiceLifetime::Transient => {
                    let factory = factory_data.factory.clone();
                    let instance = factory(self);
                    instance.downcast::<T>().ok().map(|arc| (*arc).clone())
                }
            }
        } else {
            None
        }
    }

    pub fn is_registered<T>(&self) -> bool
    where
        T: 'static,
    {
        self.factories.contains_key(&TypeId::of::<T>())
    }

    pub fn service_count(&self) -> usize {
        self.factories.len()
    }

    pub fn singleton_count(&self) -> usize {
        self.singletons.len()
    }
}

impl Default for DiContainer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    trait TestService: Send + Sync {
        fn get_name(&self) -> String;
    }

    struct TestServiceImpl {
        name: String,
    }

    impl TestService for TestServiceImpl {
        fn get_name(&self) -> String {
            self.name.clone()
        }
    }

    #[test]
    fn test_transient_service() {
        let mut container = DiContainer::new();
        container.register::<Arc<dyn TestService>>(
            |_| {
                let service = TestServiceImpl {
                    name: "transient".to_string(),
                };
                Arc::new(service) as Arc<dyn TestService>
            },
            ServiceLifetime::Transient,
        );

        let service1 = container.resolve::<Arc<dyn TestService>>();
        let service2 = container.resolve::<Arc<dyn TestService>>();

        assert!(service1.is_some(), "service1 should be Some");
        assert!(service2.is_some(), "service2 should be Some");
        assert_ne!(
            Arc::as_ptr(service1.as_ref().unwrap()),
            Arc::as_ptr(service2.as_ref().unwrap())
        );
    }

    #[test]
    fn test_singleton_service() {
        let mut container = DiContainer::new();
        container.register::<Arc<dyn TestService>>(
            |_| {
                let service = TestServiceImpl {
                    name: "singleton".to_string(),
                };
                Arc::new(service) as Arc<dyn TestService>
            },
            ServiceLifetime::Singleton,
        );

        let service1 = container.resolve::<Arc<dyn TestService>>();
        let service2 = container.resolve::<Arc<dyn TestService>>();

        assert!(service1.is_some(), "service1 should be Some");
        assert!(service2.is_some(), "service2 should be Some");
        assert_eq!(
            Arc::as_ptr(service1.as_ref().unwrap()),
            Arc::as_ptr(service2.as_ref().unwrap())
        );
    }

    #[test]
    fn test_dependent_services() {
        trait DepService: Send + Sync {
            fn get_value(&self) -> i32;
        }

        struct DepServiceImpl;

        impl DepService for DepServiceImpl {
            fn get_value(&self) -> i32 {
                42
            }
        }

        trait MainService: Send + Sync {
            fn get_dep_value(&self) -> i32;
        }

        struct MainServiceImpl {
            dep: Arc<dyn DepService>,
        }

        impl MainService for MainServiceImpl {
            fn get_dep_value(&self) -> i32 {
                self.dep.get_value()
            }
        }

        let mut container = DiContainer::new();
        container.register::<Arc<dyn DepService>>(
            |_| Arc::new(DepServiceImpl) as Arc<dyn DepService>,
            ServiceLifetime::Singleton,
        );
        container.register::<Arc<dyn MainService>>(
            |container| {
                let dep = container.resolve::<Arc<dyn DepService>>().unwrap();
                Arc::new(MainServiceImpl { dep }) as Arc<dyn MainService>
            },
            ServiceLifetime::Singleton,
        );

        let service = container.resolve::<Arc<dyn MainService>>();
        assert!(service.is_some(), "service should be Some");
        assert_eq!(service.unwrap().get_dep_value(), 42);
    }

    #[test]
    fn test_is_registered() {
        let mut container = DiContainer::new();
        assert!(!container.is_registered::<String>());

        container.register::<String>(|_| String::new(), ServiceLifetime::Transient);
        assert!(container.is_registered::<String>());
    }

    #[test]
    fn test_service_count() {
        let mut container = DiContainer::new();
        assert_eq!(container.service_count(), 0);

        container.register::<String>(|_| String::new(), ServiceLifetime::Transient);
        assert_eq!(container.service_count(), 1);

        container.register::<i32>(|_| 123, ServiceLifetime::Singleton);
        assert_eq!(container.service_count(), 2);
    }

    #[test]
    fn test_singleton_count() {
        let mut container = DiContainer::new();
        container.register::<String>(|_| String::new(), ServiceLifetime::Singleton);
        assert_eq!(container.singleton_count(), 0);

        container.resolve::<String>();
        assert_eq!(container.singleton_count(), 1);

        container.resolve::<String>();
        assert_eq!(container.singleton_count(), 1);
    }
}
